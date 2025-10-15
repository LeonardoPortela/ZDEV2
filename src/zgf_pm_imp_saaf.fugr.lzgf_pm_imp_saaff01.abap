*----------------------------------------------------------------------*
***INCLUDE LZGF_PM_IMP_SAAFF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_INSERIR_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_RETURN  text
*      -->P_C_E  text
*      -->P_028    text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
*FORM zf_inserir_mensagem USING t_return  TYPE bapiret2
FORM zf_inserir_mensagem USING  u_type    TYPE bapiret2-type
                              u_number  TYPE bapiret2-number
                              u_par_1   TYPE any
                              u_par_2   TYPE any
                              u_par_3   TYPE any
                              u_par_4   TYPE any
                        CHANGING t_return LIKE et_return.


  ls_return-type       = u_type.
  ls_return-id         = c_zppm001.
  ls_return-number     = u_number.
  ls_return-message    = u_par_3.
  ls_return-message_v1 = u_par_3.
  ls_return-message_v2 = u_par_4.
  ls_return-message_v3 = u_par_3.
  ls_return-message_v4 = u_par_4.

  CASE ls_return-number.
    WHEN 01 OR 02 OR 999.
    WHEN OTHERS.
      MESSAGE ID c_zppm001
              TYPE u_type
              NUMBER u_number
              WITH u_par_1
                   u_par_2
                   u_par_3
                   u_par_4
              INTO ls_return-message.
  ENDCASE.

  ls_return-field = |{ wa_input-i_cod_transa_saaf ALPHA = OUT }#{ wa_input-i_cod_d_transa_saaf ALPHA = OUT }#{ wa_input-i_cod_planta_transa ALPHA = OUT }|.

  APPEND ls_return TO et_return.

ENDFORM.

FORM zf_ins_mensagem USING  u_type    TYPE bapiret2-type
                              u_number  TYPE bapiret2-number
                              u_id      TYPE bapiret2-id
                              u_par_1   TYPE any
                              u_par_2   TYPE any
                              u_par_3   TYPE any
                              u_par_4   TYPE any
                        CHANGING t_return LIKE et_return.


  ls_return-type       = u_type.
  ls_return-id         = u_id.
  ls_return-number     = u_number.
  ls_return-message    = u_par_3.
  ls_return-message_v1 = u_par_3.
  ls_return-message_v2 = u_par_4.
  ls_return-message_v3 = u_par_3.
  ls_return-message_v4 = u_par_4.

  CASE ls_return-number.
    WHEN 01 OR 02 OR 999.
    WHEN OTHERS.
      MESSAGE ID u_id
              TYPE u_type
              NUMBER u_number
              WITH u_par_1
                   u_par_2
                   u_par_3
                   u_par_4
              INTO ls_return-message.
  ENDCASE.

  ls_return-field = |{ wa_input-i_cod_transa_saaf ALPHA = OUT }#{ wa_input-i_cod_d_transa_saaf ALPHA = OUT }#{ wa_input-i_cod_planta_transa ALPHA = OUT }|.

  APPEND ls_return TO et_return.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSA_CABEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_processa_cabec.

  DATA: rg_matnr     TYPE RANGE OF matnr,
        vg_matnr_aux TYPE char18.
*
  CLEAR it_dimpt[].
  CLEAR vcont.
*
  SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE tl_param
    WHERE param EQ 'TP_OBJ'
    AND abastec EQ 'X'.                                                       "/Modificação CS2016001593

  "DELETE TL_PARAM WHERE CONST <> 'FROTA' AND CONST <> 'TERCEIROS'.           "/Modificação CS2016001593
  IF wa_equi-eqtyp NE 'M'.
    READ TABLE tl_param WITH KEY zval = wa_equi-eqtyp.
    IF sy-subrc IS INITIAL.
      IF v_dt_ponto IS INITIAL.
        v_dt_ponto = sy-datum.
      ENDIF.



      CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
        EXPORTING
          i_equnr    = wa_equi-equnr
        TABLES
          et_return1 = it_return
          et_diimpt  = it_dimpt.

      APPEND LINES OF it_return[] TO et_return.

      IF it_dimpt IS INITIAL.
        PERFORM zf_inserir_mensagem USING c_e 032 space space space  space
                 CHANGING et_return.
        p_erro = abap_true.
        EXIT.
      ELSE.
**  Tratar duplicidade de pontos de medição
        LOOP AT it_dimpt INTO wa_dimpt WHERE inact = ' '.
          CASE wa_dimpt-atnam.
            WHEN 'ODOMETRO' OR 'HORIMETRO'.
** Ponto de medição usado para apontar equipamento
              IF wa_equi-hequi IS NOT INITIAL.
                ADD 1 TO vcont.
                vtpponto = wa_dimpt-atnam.
              ELSE.
                IF wa_dimpt-indtr IS INITIAL.
                  ADD 1 TO vcont.
                  vtpponto = wa_dimpt-atnam.
                ENDIF.
              ENDIF.
            WHEN 'COMBUSTIVEL'.
              v_pt_comb = wa_dimpt-point.
          ENDCASE.

        ENDLOOP.

        IF vcont EQ 1.
          IF wa_equi-hequi IS INITIAL.
            READ TABLE it_dimpt
              INTO wa_dimpt WITH KEY atnam = vtpponto
                                     inact = ' '
                                     indtr = ' '.

            v_itemo  = wa_dimpt-psort.
            v_unido  = wa_dimpt-mrngu.

            MOVE wa_dimpt-point TO v_pointo.
            CONDENSE v_pointo NO-GAPS.

**  Valida se existe ponto vida útil para equipamento
            READ TABLE it_dimpt
              INTO wa_dimpt WITH KEY trans = v_pointo.
            IF sy-subrc IS NOT INITIAL.
              ls_return-message = 'Não existe ponto de medição atribuído ao "Vida Útil" .'.
              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                          CHANGING et_return.
              p_erro = abap_true.
              EXIT.
            ENDIF.
          ENDIF.

        ELSEIF vcont GT 1.
          ls_return-message = 'Existem ponto duplicados para HORIMETRO/ODOMETRO.'.
          PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                      CHANGING et_return.
          p_erro = abap_true.
          EXIT.
        ELSEIF vcont IS INITIAL.
          ls_return-message = 'Não existem ponto válido para HORIMETRO/ODOMETRO.'.
          PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                      CHANGING et_return.
          p_erro = abap_true.
          EXIT.
        ENDIF.

**  Encontrar ponto de equipamento superior
        IF wa_equi-hequi IS NOT INITIAL.
          CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
            EXPORTING
              i_equnr    = wa_equi-hequi
            TABLES
              et_return1 = it_return
              et_diimpt  = it_dimpt_sup.

          APPEND LINES OF it_return[] TO et_return.

**  Tratar duplicidade de pontos de medição
          LOOP AT it_dimpt_sup INTO wa_dimpt WHERE inact = ' ' AND indtr = ' '.
            CASE wa_dimpt-atnam.
              WHEN 'ODOMETRO' OR 'HORIMETRO'.
                ADD 1 TO vcont_sup.
                vtpponto = wa_dimpt-atnam.
            ENDCASE.

          ENDLOOP.

          IF vcont_sup EQ 1.
            READ TABLE it_dimpt_sup
               INTO wa_dimpt WITH KEY atnam = vtpponto
                                      inact = ' '
                                      indtr = ' '.

            v_itemo  = wa_dimpt-psort.
            v_unido  = wa_dimpt-mrngu.

            MOVE wa_dimpt-point TO v_pointo.
            CONDENSE v_pointo NO-GAPS.

**  Valida se existe vida útil ligado ao vida util do equipamento superior
            READ TABLE it_dimpt
              INTO wa_dimpt WITH KEY trans = v_pointo.
            IF sy-subrc IS NOT INITIAL.
              ls_return-message =
                   'Não existe ponto de medição atribuído ao "Vida Útil" do equipamento superior.'.
              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                          CHANGING et_return.
              p_erro = abap_true.
              EXIT.
            ENDIF.

          ELSEIF vcont_sup GT 1.
            ls_return-message = 'Existem pontos duplicados para HORIMETRO/ODOMETRO no equipamento superior.'.
            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                        CHANGING et_return.
            p_erro = abap_true.
            EXIT.
          ELSE.
            ls_return-message = 'Não existem pontos válido para HORIMETRO/ODOMETRO no equipamento superior.'.
            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                        CHANGING et_return.
            p_erro = abap_true.
            EXIT.
          ENDIF.

        ENDIF.

      ENDIF.

    ELSE.
      ls_return-message = 'Equipamento ou frota inválido'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.
      p_erro = abap_true.
      EXIT.
    ENDIF.

**  Verificar se existe alguma medição válida para o ponto a ser apontado
    CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
      EXPORTING
        buffer_bypass  = ' '
        dyfield        = ' '
        offset_date    = v_dt_ponto
        offset_time    = v_hr_ponto
        point          = v_pointo
      IMPORTING
        imrg_wa        = wa_value
      EXCEPTIONS
        imrg_not_found = 1
        OTHERS         = 2.

    IF sy-subrc IS INITIAL.
** Calcula posição do contador
      PERFORM zf_convert USING wa_value-recdu wa_value-readg 'X' vl_cont.
      REPLACE ALL OCCURRENCES OF ',' IN vl_cont WITH '.' .
      MOVE vl_cont TO v_medano.

    ELSE.
      ls_return-message =
           'Não existe nenhuma medição inicial para este equipamento.'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

** Valida centro
** Seleciona o centro mas de acordo com o valor definido no campo "Centro Planejamento" na
** transação IE03, aba Organização.
  MOVE wa_equi-iwerk TO wa_equi-swerk.

  IF NOT wa_input-s_cod_ponto_abast IS INITIAL.
    PERFORM zf_valida_posto
      USING wa_equi-swerk wa_input-s_cod_ponto_abast
      CHANGING p_erro.

    IF p_erro IS INITIAL.
      SELECT SINGLE type_text
       FROM t370fld_stn_t
       INTO wa_comb-postot
       WHERE station  = wa_input-s_cod_ponto_abast
         AND lang_key = sy-langu.

      wa_comb-posto = wa_input-s_cod_ponto_abast.

    ELSE.
      CLEAR: wa_comb-posto, wa_comb-postot.
    ENDIF.

  ENDIF.

  IF NOT wa_input-s_cod_combust IS INITIAL.
    PERFORM zf_valida_mat
      USING
        'O'
      CHANGING
        wa_input-s_cod_combust.

    wa_comb-comb = wa_input-s_cod_combust.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wa_comb-comb
*      IMPORTING
*        output = lv_matnr.


    "======================================================================Ajuste realizado teste integração projeto SAP Hana / AOENNING.

    CLEAR: vg_matnr_aux.
    FREE: rg_matnr.

    IF wa_comb-comb IS NOT INITIAL.
      vg_matnr_aux = wa_comb-comb.
      vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ wa_comb-comb ALPHA = IN }| ) TO rg_matnr.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ vg_matnr_aux }| ) TO rg_matnr.
    ENDIF.

    SELECT SINGLE maktx
      FROM makt
      INTO wa_comb-combt
     WHERE matnr IN rg_matnr
       AND spras = sy-langu.

  ENDIF.

  IF NOT wa_comb-local IS INITIAL.
    SELECT SINGLE pltxt
     FROM iflo
     INTO wa_comb-localt
     WHERE tplnr = wa_comb-local
      AND spras = sy-langu
      AND fltyp EQ 'C'.
  ENDIF.


  IF NOT wa_equi-equnr IS INITIAL.
    PACK wa_equi-equnr TO v_equnr.
    CONDENSE v_equnr NO-GAPS.

  ENDIF.

  IF p_erro = 'X'.
    CLEAR: wa_saida1, it_saida1[].
    CLEAR: wa_saida2, it_saida2[].

    CLEAR: wa_comb, v_erro, v_erroo, v_erroh, v_equip,
           wa_equi, v_equnr, v_pointo, v_itemo, v_medano,
           v_medato, v_unido, v_pointh, v_itemh, v_medanh,
           v_medath, v_unidh, v_dt_ponto, v_hr_ponto, v_hr_pto,
           v_nbaixa, v_equnr_sup, v_super_desc, v_n_form.
*
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSA_COMPARTIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_processa_compartimento .
*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_equip
    IMPORTING
      output = v_equip.

  CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
    EXPORTING
      i_equnr   = v_equip
    TABLES
      et_diimpt = it_dimpt.

  READ TABLE it_dimpt INTO wa_dimpt WITH KEY locas = v_locas.
  IF sy-subrc IS INITIAL.
    IF wa_dimpt-mptyp EQ c_f.       "Filtro
      PERFORM zf_changes_filtro.
    ELSEIF wa_dimpt-mptyp EQ c_h.   "Óleo
      PERFORM zf_changes_oleo.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_EQUIPAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_verifica_equipamento .
*
  CLEAR: wa_equi, lv_equip, wa_return, lv_valr1, lv_valor,
         wa_dimpt, v_medano, v_medanh, wa_status, lv_valr2,
         lv_tabix, vtpponto, p_erro, lv_matnr, v_equnr_sup,
         v_super_desc, v_dt_ponto, v_hr_ponto.

  CLEAR: wa_saida1, it_saida1[].
  CLEAR: wa_saida2, it_saida2[].

  lv_reader = wa_input-s_cod_abastecedor.
  lv_operacao = wa_input-s_cod_operacao.

* Verifica se o registro atual será processado com o mesmo Hodometro/Horimetro
  IF lv_dt_inicio_mov IS INITIAL AND
     lv_s_hora_inicio_mov IS INITIAL.
    lv_dt_inicio_mov = wa_input-dt_inicio_mov.
    lv_s_hora_inicio_mov = wa_input-s_hora_inicio_mov.
  ELSEIF lv_dt_inicio_mov EQ wa_input-dt_inicio_mov AND
         lv_s_hora_inicio_mov EQ wa_input-s_hora_inicio_mov.
    CLEAR: wa_input-f_hodometro, wa_input-f_horimetro.
    lv_ja_apontou = abap_true.
  ELSE.
    lv_dt_inicio_mov = wa_input-dt_inicio_mov.
    lv_s_hora_inicio_mov = wa_input-s_hora_inicio_mov.
  ENDIF.

  FREE: gt_msg, it_return, it_dimpt, it_retmed, it_dimpt_sup.
*
  CONDENSE: v_equip, v_equnr NO-GAPS.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
    EXPORTING
      input                     = wa_input-dt_inicio_mov
    IMPORTING
      output                    = v_dt_ponto
    EXCEPTIONS
      plausibility_check_failed = 1
      wrong_format_in_input     = 2.

  IF v_dt_ponto IS INITIAL.
    v_dt_ponto = wa_input-dt_inicio_mov.
  ENDIF.

  wa_comb-quant = wa_input-f_qtde_abast.    "Combustível
  wa_comb-local = wa_input-s_cod_local_abastec.  "Local

  IF wa_input-s_tipo_abastec = 'M'.
    IF wa_input-i_cod_motivo_abastec_man IS INITIAL. "//Erros no DIV, ou seja, sem Horímetro/Odometro
      v_erro = abap_true.
    ELSE.
      v_nerro = abap_true.
    ENDIF.
  ENDIF.
*
*  converte hora da tela
  IF wa_input-s_hora_inicio_mov IS INITIAL.
    CONCATENATE sy-uzeit+0(2) ':' sy-uzeit+2(2) INTO v_hr_pto.
  ELSE.
    CONCATENATE wa_input-s_hora_inicio_mov(2) ':' wa_input-s_hora_inicio_mov+2(2) INTO v_hr_pto.
  ENDIF.

  lv_hr_c = v_hr_pto.
  REPLACE ALL OCCURRENCES OF ':' IN lv_hr_c WITH ''.
  CONCATENATE lv_hr_c '00' INTO lv_hr_c.

  CALL FUNCTION 'MOVE_CHAR_TO_NUM'
    EXPORTING
      chr             = lv_hr_c
    IMPORTING
      num             = lv_hr_i
    EXCEPTIONS
      convt_no_number = 1
      convt_overflow  = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    MOVE lv_hr_c TO v_hr_ponto.
  ENDIF.

  v_equnr = v_equip.

**  Buscar dados do equipamento
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_equip
    IMPORTING
      output = lv_equip.
  "*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
  CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      equipment         = lv_equip
    IMPORTING
      data_general_exp  = wl_general_equi
      data_specific_exp = wl_specific_equi
      return            = wl_return.

  IF wl_return IS INITIAL.
    wa_equi-equnr = v_equip.
    wa_equi-eqktx = wl_general_equi-descript.
    wa_equi-swerk = wl_general_equi-maintplant.
    wa_equi-daufn = wl_general_equi-standorder.
    MOVE wl_general_equi-settlorder TO wa_equi-aufnr.
    wa_equi-eqart = wl_general_equi-objecttype.
    wa_equi-eqtyp = wl_specific_equi-equicatgry.
    wa_equi-iwerk = wl_general_equi-planplant.
    wa_equi-hequi = wl_specific_equi-read_supeq.

**  Dados de equipamento superior
    IF wa_equi-hequi IS NOT INITIAL.
      CLEAR: wl_general_equi, wl_specific_equi, wl_return.

      "*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          equipment         = wa_equi-hequi
        IMPORTING
          data_general_exp  = wl_general_equi
          data_specific_exp = wl_specific_equi
          return            = wl_return.

      v_equnr_sup   = wa_equi-hequi.
      SHIFT v_equnr_sup LEFT DELETING LEADING '0'.
      v_super_desc  = wl_general_equi-descript.

    ENDIF.

  ELSE.
    wl_return-field = |{ wa_input-i_cod_transa_saaf ALPHA = OUT }#{ wa_input-i_cod_d_transa_saaf ALPHA = OUT }#{ wa_input-i_cod_planta_transa ALPHA = OUT }|.
    APPEND wl_return TO et_return.
*    PERFORM ZF_INSERIR_MENSAGEM USING  C_E 031 SPACE SPACE SPACE  SPACE
*                         CHANGING ET_RETURN.
    p_erro = 'X'.
    EXIT.
  ENDIF.

  SELECT SINGLE zval
    FROM ztparam
    INTO lv_valor
   WHERE param = 'CONT'
     AND const = 'HORIMETRO'.

  SELECT SINGLE zval
    FROM ztparam
    INTO lv_valr1
   WHERE param = 'CONT'
     AND const = 'ODOMETRO'.
* Processar retgistro
  PERFORM zf_processa_registro.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSA_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_processa_registro .
*
* Verifica se é lubrificante ou filtro.
  IF NOT wa_input-s_cod_compartimento IS INITIAL.
    v_locas = wa_saida1-sistema = wa_saida2-sistema = wa_input-s_cod_compartimento.
    PERFORM zf_processa_compartimento.
  ENDIF.
*
  PERFORM zf_processa_cabec.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CHANGES_FILTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_changes_filtro .

  DATA: lv_code_ref TYPE matnr,
        lv_matnr    TYPE matnr,
        lv_val      TYPE ztparam-zval,
        lv_val1     TYPE ztparam-zval,
        lv_code     TYPE zedft_atividade,
        lv_unid     TYPE mara-meins,
        ls_qpct     TYPE ty_qpct,
        p_erro(1).

  " Atividade e Descrição
  IF wa_input-i_troca_limpeza_filtro EQ c_1 OR  " troca
     wa_input-i_troca_limpeza_filtro EQ c_2.    " Limpeza Filtro

    CLEAR: lv_val, lv_val1.
    wa_saida2-readg = wa_input-f_qtde_produto.  "Filtro

    CASE wa_input-ib_produto_remonta.
      WHEN c_1. lv_code = c_0010.        " Troca
      WHEN c_2. lv_code = c_0030.        " Limpeza
    ENDCASE.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val
    WHERE param = 'ATIV_FILT'
      AND const = 'CATALOG'.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val1
    WHERE param = 'ATIV_FILT'
      AND const = 'CATGROUP'.

    SELECT SINGLE kurztext
    FROM qpct
    INTO ls_qpct-kurztext
    WHERE katalogart = lv_val
      AND code       = lv_code
      AND codegruppe = lv_val1
      AND sprache    = sy-langu.

    TRANSLATE ls_qpct-kurztext TO UPPER CASE.
*    IF WA_SAIDA2-CODE_TEXT <> LS_QPCT-KURZTEXT AND
*       WA_SAIDA2-CODE_TEXT IS NOT INITIAL.
*      CLEAR WA_SAIDA2-ORDEM.
*    ENDIF.
    wa_saida2-code_text = ls_qpct-kurztext.
    wa_saida2-code      = lv_code.

    IF sy-subrc <> 0.
      PERFORM zf_inserir_mensagem USING  c_e 028 space space space  space
                     CHANGING et_return.
      CLEAR wa_saida2-code_text.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

  " Filtro e Descrição
  PERFORM zf_valida_mat
    USING
      'F'
    CHANGING
      wa_input-s_cod_produto.

  IF NOT wa_input-s_cod_produto IS INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wa_input-s_cod_produto
*      IMPORTING
*        output = lv_code_ref.

    CLEAR: vg_matnr_aux.
    FREE: rg_matnr.


    IF wa_input-s_cod_produto IS NOT INITIAL.
      vg_matnr_aux = wa_input-s_cod_produto.
      vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ wa_input-s_cod_produto ALPHA = IN }| ) TO rg_matnr.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ vg_matnr_aux }| ) TO rg_matnr.
    ENDIF.

    wa_saida2-code_ref = wa_input-s_cod_produto.

    SELECT SINGLE maktx
    INTO wa_saida2-desc_ref
    FROM makt
    WHERE matnr IN rg_matnr.

    " Unidade
    SELECT SINGLE meins
    FROM mara
    INTO lv_unid
    WHERE matnr IN rg_matnr.
    IF sy-subrc = 0.
      wa_saida2-mrngu = lv_unid.
    ENDIF.
  ENDIF.

  " Motivo e descrição
  IF NOT wa_input-s_cod_motivo_troca_remonta IS INITIAL.
    CLEAR: lv_val, lv_val1.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val
    WHERE param = 'MOTIV_FILT'
      AND const = 'CATALOG'.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val1
    WHERE param = 'MOTIV_FILT'
      AND const = 'CATGROUP'.

    SELECT SINGLE kurztext
    FROM qpct
    INTO wa_saida2-desc_motivo
    WHERE code       = wa_input-s_cod_motivo_troca_remonta
      AND sprache    = sy-langu
      AND katalogart = lv_val
      AND codegruppe = lv_val1.

    IF sy-subrc <> 0.
      PERFORM zf_inserir_mensagem USING  c_e 029 space space space  space
                     CHANGING et_return.
      CLEAR wa_saida2-motivo.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ELSEIF wa_input-s_cod_motivo_troca_remonta  IS INITIAL AND
         v_locas                      IS NOT INITIAL AND
         lv_code                      IS NOT INITIAL AND
         wa_input-s_cod_ponto_abast   IS NOT INITIAL AND
         wa_saida2-ordem          IS NOT INITIAL.
    PERFORM zf_inserir_mensagem USING  c_e 030 space space space  space
               CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  " Posto e Descrição

  IF NOT wa_input-s_cod_ponto_abast IS INITIAL.
    PERFORM zf_valida_posto
      USING wa_equi-swerk wa_input-s_cod_ponto_abast
      CHANGING p_erro.

    IF p_erro IS INITIAL.

      wa_saida2-posto = wa_input-s_cod_ponto_abast.

      SELECT SINGLE type_text
        FROM t370fld_stn_t
        INTO wa_saida2-desc_posto
       WHERE station  = wa_saida2-posto
         AND lang_key = sy-langu.
    ELSE.
      CLEAR: wa_saida2-posto, wa_saida1-desc_posto.
    ENDIF.
  ENDIF.

  "Ordem
  IF wa_saida2-code = '0030'.
    PACK wa_equi-aufnr TO wa_saida2-ordem.
  ELSE.
    PACK wa_input-ib_ordem TO wa_saida2-ordem.
  ENDIF.
  CONDENSE wa_saida2-ordem  NO-GAPS.

*
  wa_saida2-motivo   = wa_input-s_cod_motivo_troca_remonta.
*
  PERFORM zf_valida_sistema
    USING 'F' wa_saida2-sistema wa_saida2-ordem
    CHANGING wa_saida2-sistema
             wa_saida2-ordem.
  IF wa_saida2-sistema IS NOT INITIAL.
    PERFORM zf_valida_ordem
      USING wa_saida2-ordem 'F' wa_saida2-code wa_saida2-sistema
      CHANGING wa_saida2-ordem
               wa_saida2-plano.
  ENDIF.
*
  IF NOT p_erro = abap_true.
    APPEND wa_saida2 TO it_saida2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CHANGES_OLEO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_changes_oleo .

  DATA: ls_qpct     TYPE ty_qpct,
        lv_matnr    TYPE matnr,
        lv_code_ref TYPE matnr,
        lv_val      TYPE ztparam-zval,
        lv_val1     TYPE ztparam-zval,
        lv_code     TYPE zedft_atividade,
        lv_unid     TYPE mara-meins,
        p_erro(1).

  " Atividade e Descrição (Troca ou Susbt / Remonta / Análise
  IF NOT wa_input-ib_produto_remonta IS INITIAL.
    CASE wa_input-ib_produto_remonta.
      WHEN c_0. lv_code = c_0010.       " Troca
      WHEN c_1. lv_code = c_0020.       " Remonta
      WHEN c_3. lv_code = c_0040.       " Análise
    ENDCASE.
    CLEAR: lv_val, lv_val1.

    wa_saida1-readg = wa_input-f_qtde_produto.  "Luvrificante

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val
    WHERE param = 'ATIV_OLEO'
      AND const = 'CATALOG'.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val1
    WHERE param = 'ATIV_OLEO'
      AND const = 'CATGROUP'.

    SELECT SINGLE code kurztext
    FROM qpct
    INTO ls_qpct
    WHERE katalogart = lv_val
*      AND CODE       = WA_SAIDA1-CODE
      AND code       = lv_code
      AND codegruppe = lv_val1
      AND sprache    = sy-langu.

    IF sy-subrc = 0.
      TRANSLATE ls_qpct-kurztext TO UPPER CASE.
*      IF WA_SAIDA1-CODE_TEXT <> LS_QPCT-KURZTEXT AND
*         WA_SAIDA1-CODE_TEXT IS NOT INITIAL.
*        CLEAR WA_SAIDA1-ORDEM.
*      ENDIF.
      wa_saida1-code_text = ls_qpct-kurztext.
      wa_saida1-code      = lv_code.
    ELSE.
      PERFORM zf_inserir_mensagem USING  c_e 028 space space space  space
                     CHANGING et_return.
      CLEAR: wa_saida1-code_text, wa_saida1-code.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.

*  Ordem
  IF lv_code = c_0020.
    PACK wa_equi-aufnr TO wa_saida1-ordem. "CLEUDO vai ver
  ELSE.
    PACK wa_input-ib_ordem TO wa_saida1-ordem.
  ENDIF.
  CONDENSE wa_saida1-ordem  NO-GAPS.
*
*  Compartimento e Descrição
  PERFORM zf_valida_mat
    USING
      'C'
    CHANGING
      wa_input-s_cod_produto.

  IF NOT wa_input-s_cod_produto IS INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wa_input-s_cod_produto
*      IMPORTING
*        output = lv_code_ref.

*    wa_saida1-code_ref = wa_input-s_cod_produto.

    CLEAR: vg_matnr_aux.
    FREE: rg_matnr.

    IF wa_input-s_cod_produto IS NOT INITIAL.
      vg_matnr_aux = wa_input-s_cod_produto.
      vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ wa_input-s_cod_produto ALPHA = IN }| ) TO rg_matnr.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ vg_matnr_aux }| ) TO rg_matnr.
    ENDIF.

    SELECT SINGLE maktx
    INTO wa_saida1-desc_ref
    FROM makt
    WHERE matnr IN rg_matnr.

    " Unidade
    SELECT SINGLE meins
    FROM mara
    INTO lv_unid
    WHERE matnr IN rg_matnr.

    IF sy-subrc = 0.
      wa_saida1-mrngu = lv_unid.
    ENDIF.
  ENDIF.

  " Posto e Descrição
  IF NOT wa_input-s_cod_ponto_abast  IS INITIAL.
    PERFORM zf_valida_posto
      USING wa_equi-swerk wa_input-s_cod_ponto_abast
      CHANGING p_erro.

    IF p_erro IS INITIAL.

      wa_saida1-posto = wa_input-s_cod_ponto_abast.

      SELECT SINGLE type_text
        FROM t370fld_stn_t
        INTO wa_saida1-desc_posto
       WHERE station  = wa_saida1-posto
         AND lang_key = sy-langu.
    ELSE.
      CLEAR: wa_saida1-posto, wa_saida1-desc_posto.
    ENDIF.
  ENDIF.

  " Motivo e descrição
  CLEAR: lv_val, lv_val1.
  IF NOT wa_input-s_cod_motivo_troca_remonta IS INITIAL.
    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val
    WHERE param = 'MOTIV_OLEO'
      AND const = 'CATALOG'.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_val1
    WHERE param = 'MOTIV_OLEO'
      AND const = 'CATGROUP'.

    SELECT SINGLE kurztext
    FROM qpct
    INTO wa_saida1-desc_motivo
    WHERE code       = wa_input-s_cod_motivo_troca_remonta
      AND sprache    = sy-langu
      AND katalogart = lv_val
      AND codegruppe = lv_val1.

    IF sy-subrc <> 0.
      PERFORM zf_inserir_mensagem USING  c_e 029 space space space  space
                     CHANGING et_return.
      CLEAR wa_saida1-motivo.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ELSEIF wa_input-s_cod_motivo_troca_remonta  IS INITIAL AND
         v_locas                      IS NOT INITIAL AND
         lv_code                      IS NOT INITIAL AND
         wa_input-s_cod_ponto_abast   IS NOT INITIAL AND
         wa_saida1-ordem   IS NOT INITIAL.
    PERFORM zf_inserir_mensagem USING  c_e 030 space space space  space
               CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.
*
  wa_saida1-motivo      = wa_input-s_cod_motivo_troca_remonta.
*
*  Sistema
  PERFORM zf_valida_sistema
    USING 'H' wa_saida1-sistema wa_saida1-ordem
    CHANGING wa_saida1-sistema
             wa_saida1-ordem.

*   Numero da ordem
  IF wa_saida1-sistema IS NOT INITIAL.
    PERFORM zf_valida_ordem
      USING wa_saida1-ordem 'O' wa_saida1-code wa_saida1-sistema
      CHANGING wa_saida1-ordem
               wa_saida1-plano.
  ENDIF.
*
  IF NOT p_erro = abap_true.
    APPEND wa_saida1 TO it_saida1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1550   text
*      <--P_WA_INPUT_S_COD_PRODUTO  text
*----------------------------------------------------------------------*
FORM zf_valida_mat USING p_tipo  TYPE c
                   CHANGING c_matnr.

  DATA: rg_matnr     TYPE RANGE OF matnr,
        vg_matnr_aux TYPE char18.

  DATA: vl_matnr TYPE mara-matnr.

  IF c_matnr IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = c_matnr
*      IMPORTING
*        output = vl_matnr.

    CLEAR: vg_matnr_aux.
    vg_matnr_aux = c_matnr.
    vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.


    "======================================================================Ajuste realizado teste integração projeto SAP Hana / AOENNING.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ c_matnr ALPHA = IN }| ) TO rg_matnr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ vg_matnr_aux }| ) TO rg_matnr.

    SELECT mara~matnr
      INTO TABLE it_mara
      FROM mara
     INNER JOIN ztftpm_lubri ON matnr = ztftpm_lubri~conjunto
     WHERE ztftpm_lubri~categoria = p_tipo
       AND matnr IN rg_matnr.


    IF sy-subrc IS NOT INITIAL.
      ls_return-message = |Material { vl_matnr } inválido.|.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
               CHANGING et_return.
      p_erro = abap_true.
      EXIT.

      CLEAR c_matnr.
    ENDIF.
  ENDIF.
ENDFORM.                    "ZF_VALIDA_MAT
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_POSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EQUI_SWERK  text
*      -->P_WA_INPUT_S_COD_PONTO_ABAST  text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_valida_posto USING v_centro v_posto CHANGING c_erro.
  DATA: lv_station TYPE t370fld_mat-station,
        lv_centro  TYPE t370fld_stn-plant,
        lv_posto   TYPE t370fld_stn-station.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_centro
    IMPORTING
      output = lv_centro.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_posto
    IMPORTING
      output = lv_posto.

  SELECT SINGLE station
    FROM t370fld_stn
    INTO lv_station
   WHERE t370fld_stn~plant = lv_centro
     AND t370fld_stn~station = lv_posto.

  IF lv_station IS INITIAL.
    ls_return-message = 'Posto inválido.'.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
             CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.
ENDFORM.                    "zf_valida_posto
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_SISTEMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1759   text
*      -->P_WA_SAIDA1_SISTEMA  text
*      -->P_WA_SAIDA1_ORDEM  text
*      <--P_WA_SAIDA1_SISTEMA  text
*      <--P_WA_SAIDA1_ORDEM  text
*----------------------------------------------------------------------*
FORM zf_valida_sistema
  USING p_tipo     TYPE c
        p_sistema  TYPE zftpme_lubrificante-sistema
        p_ordem    TYPE v_equi-aufnr
  CHANGING p_locas TYPE zftpme_lubrificante-sistema
           p_ord_s TYPE zftpme_lubrificante-ordem.

  DATA: vl_equip  TYPE          v_equi-equnr,
        tl_diimpt TYPE TABLE OF diimpt WITH HEADER LINE.

  DATA: tl_ranges  TYPE bapi_alm_order_listhead_ranges OCCURS 0,
        tl_result  LIKE bapi_alm_order_listhead_result OCCURS 0,
        tl_return  LIKE bapiret2 OCCURS 0,
        wl_ranges  TYPE bapi_alm_order_listhead_ranges,
        wl_result  TYPE bapi_alm_order_listhead_result,
        vl_cont    TYPE i,
        vl_ordem   TYPE caufv-aufnr,
        lv_msg     TYPE c LENGTH 150,
        lv_num_lin TYPE i.

  IF p_sistema IS NOT INITIAL.
    vl_equip = v_equip.

    REFRESH: tl_diimpt.

    tl_diimpt[] = it_dimpt[].

***  Valida duplicidade de sistemas em pontos.
    DELETE tl_diimpt WHERE locas <> p_sistema.
    DELETE tl_diimpt WHERE mptyp <> p_tipo.

    DESCRIBE TABLE tl_diimpt LINES lv_num_lin.

    READ TABLE tl_diimpt WITH KEY locas = p_sistema
                                  mptyp = p_tipo.

    IF sy-subrc <> 0.
      ls_return-message = 'Sistema inválido.'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
               CHANGING et_return.
      p_erro = abap_true.
      EXIT.
      CLEAR: p_locas.
    ELSEIF lv_num_lin > 1.
      CONCATENATE 'Pontos duplicados para o sistema (' p_sistema ')' INTO lv_msg.
      CLEAR: p_locas.
      ls_return-message = lv_msg.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
               CHANGING et_return.
      p_erro = abap_true.
      EXIT.
    ELSE.
      IF p_ordem IS NOT INITIAL.
        wl_ranges-field_name = 'SHOW_COMPLETED_DOCUMENTS'.
        wl_ranges-sign = 'I'.
        wl_ranges-option = 'BT'.

        APPEND wl_ranges TO tl_ranges.

        "*---> 28/06/2023 - Migração S4 - LO -> não foram usadas listas de objetos e material
        CALL FUNCTION 'BAPI_ALM_ORDERHEAD_GET_LIST' "#EC CI_USAGE_OK[2669857]
          TABLES                                   "#EC CI_USAGE_OK[2438131]
            it_ranges = tl_ranges
            et_result = tl_result
            return    = tl_return
          EXCEPTIONS
            OTHERS    = 1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = p_ordem
          IMPORTING
            output = vl_ordem.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = v_equip
          IMPORTING
            output = vl_equip.

        READ TABLE tl_result INTO wl_result WITH KEY orderid   = vl_ordem
                                                     equipment = vl_equip.
        IF sy-subrc = 0.
          IF wl_result-maintplan IS NOT INITIAL.
            IF wl_result-assembly <> p_sistema.
              ls_return-message = 'Sistema incopatível com a ordem selecionada.'.
              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                       CHANGING et_return.
              p_erro = abap_true.
              EXIT.
              CLEAR: p_ord_s.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      p_locas = p_sistema.
    ENDIF.
  ENDIF.
ENDFORM.                    " ZF_VALIDA_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA1_ORDEM  text
*      -->P_1774   text
*      -->P_WA_SAIDA1_CODE  text
*      -->P_WA_SAIDA1_SISTEMA  text
*      <--P_WA_SAIDA1_ORDEM  text
*      <--P_WA_SAIDA1_PLANO  text
*----------------------------------------------------------------------*
FORM zf_valida_ordem
  USING    VALUE(p_ordem)
           VALUE(p_tipo)
           VALUE(p_ativ)
           p_sistema TYPE zftpme_lubrificante-sistema
  CHANGING p_ord_s TYPE zftpme_lubrificante-ordem
           p_pln_s TYPE caufvd-warpl.

  DATA: vl_cont  TYPE i,
        vl_ordem TYPE caufv-aufnr,
        vl_equip TYPE c LENGTH 18.

  DATA: BEGIN OF wl_afih,
          warpl TYPE afih-warpl,
          auart TYPE aufk-auart,
          objnr TYPE aufk-objnr,
          sttxt TYPE bsvx-sttxt,
        END OF wl_afih.

  REFRESH it_ranges.

  IF p_ordem IS NOT INITIAL.
*    wa_ranges-field_name = 'SHOW_COMPLETED_DOCUMENTS'.
*    wa_ranges-sign = 'I'.
*    wa_ranges-option = 'EQ'.
*    APPEND wa_ranges TO it_ranges.
*
*    CALL FUNCTION 'BAPI_ALM_ORDERHEAD_GET_LIST'
*      TABLES
*        it_ranges = it_ranges
*        et_result = it_result
*        return    = it_return
*      EXCEPTIONS
*        OTHERS    = 01.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_ordem
      IMPORTING
        output = vl_ordem.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_equip
      IMPORTING
        output = vl_equip.

    SELECT SINGLE auart warpl objnr
      INTO CORRESPONDING FIELDS OF wl_afih
      FROM aufk
      INNER JOIN afih ON afih~aufnr = aufk~aufnr
      WHERE afih~aufnr = vl_ordem AND
            equnr = vl_equip.

    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        objnr            = wl_afih-objnr
        spras            = sy-langu
        flg_user_stat    = 'X'
      IMPORTING
        line             = wl_afih-sttxt
*       user_line        = <ls_object_tab>-ustxt
      EXCEPTIONS
        object_not_found = 01.

    vl_cont = 0.

    IF sy-subrc = 0.
*        LOOP AT it_result INTO wa_result WHERE equipment = vl_equip
*                                           AND orderid   = vl_ordem.
*        case p_tipo.
*          when 'O'.    "Oleo
*            CASE wa_result-order_type.
*              WHEN 'ZPM1' OR 'ZPM2'." OR 'ZPM5'.
*                IF p_ativ = '0010'.
*                  ADD 1 TO vl_cont.
*                  p_pln_s = wa_result-maintplan.
*                  p_ord_s = p_ordem.
*                ENDIF.
*              WHEN 'ZPM5'.
*                ADD 1 TO vl_cont.
*                p_pln_s = wa_result-maintplan.
*                p_ord_s = p_ordem.
*            ENDCASE.
*          when 'F'. "Filtro
*            CASE wa_result-order_type.
*              WHEN 'ZPM1' OR 'ZPM2' OR 'ZPM5'.
*                ADD 1 TO vl_cont.
*                p_pln_s = wa_result-maintplan.
*                p_ord_s = p_ordem.
*            ENDCASE.
*          when 'A'. "Abastecimento
*            IF wa_result-order_type = 'ZPM6'.
*              ADD 1 TO vl_cont.
*              p_pln_s = wa_result-maintplan.
*              p_ord_s = p_ordem.
*            ENDIF.
*          ENDcase.
*        ENDLOOP.
      SEARCH wl_afih-sttxt FOR 'LIB'.
      IF sy-subrc = 0.
        CASE p_tipo.
          WHEN 'O'.    "Oleo
            CASE wl_afih-auart.
              WHEN 'ZPM1' OR 'ZPM2'." OR 'ZPM5'.
                IF p_ativ = '0010'.
                  ADD 1 TO vl_cont.
                  p_pln_s = wl_afih-warpl.
                  p_ord_s = p_ordem.
                ENDIF.
              WHEN 'ZPM3'.                                                    "/Modificação CS2016001593
                IF p_ativ = '0040'.                                           "/Modificação CS2016001593
                  ADD 1 TO vl_cont.                                           "/Modificação CS2016001593
                  p_pln_s = wl_afih-warpl.                                    "/Modificação CS2016001593
                  p_ord_s = p_ordem.                                          "/Modificação CS2016001593
                ENDIF.
              WHEN 'ZPM5'.
                ADD 1 TO vl_cont.
                p_pln_s = wl_afih-warpl.
                p_ord_s = p_ordem.
            ENDCASE.
          WHEN 'F'. "Filtro
            CASE wl_afih-auart.
              WHEN 'ZPM1' OR 'ZPM2' OR 'ZPM5'.
                ADD 1 TO vl_cont.
                p_pln_s = wl_afih-warpl.
                p_ord_s = p_ordem.
            ENDCASE.
          WHEN 'A'. "Abastecimento
            IF wl_afih-auart = 'ZPM6'.
              ADD 1 TO vl_cont.
              p_pln_s = wl_afih-warpl.
              p_ord_s = p_ordem.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.

    IF vl_cont = 0.
      CLEAR: p_ord_s, p_pln_s.
      ls_return-message = 'Ordem não possível de realizar apontamento.'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
              CHANGING et_return.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    "zf_valida_ordem
*&---------------------------------------------------------------------*
*&      Form  ZF_CONVERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VALUE_RECDU  text
*      -->P_WA_VALUE_READG  text
*      -->P_0499   text
*      -->P_VL_CONT  text
*----------------------------------------------------------------------*
FORM zf_convert USING f_unit   TYPE c
                     f_input  LIKE impt-pyear
                     f_indik  LIKE impt-pyeari
                     f_flstr  LIKE rihimrg-pyeac.

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      char_unit       = f_unit
      decimals        = 2
      exponent        = 0
      fltp_value_si   = f_input
      indicator_value = f_indik
    IMPORTING
      char_value      = f_flstr
    EXCEPTIONS
      no_unit_given   = 01.
ENDFORM.                    " F_CONVERT
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_save .

  DATA: lv_erro        TYPE c LENGTH 1,
        lv_error       TYPE c LENGTH 1,
        lv_error2      TYPE c LENGTH 1,
        lv_erro_bapi   TYPE c LENGTH 1,
        lt_log         LIKE it_retmed,
        lv_hod         TYPE ztparam-zval,
        difd           TYPE p,
        difh           TYPE p,
        lv_hor         TYPE i,
        lv_resp        TYPE c,
        lv_difer       TYPE zedft_quantidade,
        lv_data        TYPE sy-datum,
        lv_hora        TYPE sy-uzeit,
        lv_equipamento TYPE equi-equnr,
        lv_msg         TYPE c LENGTH 255,
        lv_dias_aberto TYPE p,
        vg_bukrs       TYPE j_1bbranch-branch,
        e_status(1),
        e_messa(64).

  DATA: tl_filtro TYPE TABLE OF ty_zftpme_filtros,
        tl_oleo   TYPE TABLE OF ty_zftpme_lubrificante,
        wl_comb   TYPE ty_comb.

  FREE: it_not,it_retmed, lt_log, it_retmed1, it_ret, t_doc_med.

  CLEAR: wg_erro, wa_retmed, lv_erro, lv_error, lv_hod,
         lv_hor, lv_resp, lv_difer, lv_error2, wa_ret, wl_comb, p_erro.

  IF NOT wa_input-f_horimetro IS INITIAL.
    v_medato = wa_input-f_horimetro.
  ELSEIF NOT wa_input-f_hodometro IS INITIAL.
    v_medato = wa_input-f_hodometro.
  ENDIF.

  SELECT SINGLE bukrs
    INTO vg_bukrs
    FROM j_1bbranch
  WHERE branch = wa_equi-swerk.

  IF sy-subrc = 0.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = vg_bukrs
        i_data   = v_dt_ponto
      IMPORTING
        e_status = e_status
        e_messa  = e_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF  e_status = 'E'.
      MOVE e_messa TO ls_return-message.
      PERFORM zf_inserir_mensagem USING  c_e 999 space space ls_return-message  space
                     CHANGING et_return.
      p_erro = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF gt_msg IS NOT INITIAL.
    ls_return-message = 'O equipamento possui inconsistências. Para fazer qualquer apontamento primeiro os corrija'.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.
    p_erro = 'X'.
    EXIT.
  ENDIF.

  IF v_nbaixa IS NOT INITIAL.
    ls_return-message =
      'A opção "Não baixar materiais" foi marcada. Tem certeza que deseja não efetuar a baixa de estoque?'.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.
    p_erro = abap_true.
    EXIT.
    IF v_ans <> 1.
      p_erro = 'X'.
      EXIT.
    ELSE.
***   Gera Log
      ls_return-message =
        'Alerta: A opção "Não baixar materiais" foi marcada'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.
      p_erro = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

** Checa se há nota de odometro/horimetro aberta para o equipamento
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_equip
    IMPORTING
      output = lv_equipamento.

*Verificar categoria do equipamento para fazer o check de apontamento hora/km

  SELECT SINGLE *
  FROM v_equi
  INTO @DATA(w_categoria)
    WHERE equnr EQ @lv_equipamento.

  IF w_categoria IS NOT INITIAL AND w_categoria-eqtyp NE 'M'.
*
    IF lv_ja_apontou IS INITIAL.
* Se já houve apontamento para esta data/hora, pula esta verificação
*
      CALL FUNCTION 'BAPI_ALM_NOTIF_LIST_EQUI'
        EXPORTING
          equipment         = lv_equipamento
          notification_date = '01011900'
        TABLES
          notification      = it_not
        EXCEPTIONS
          OTHERS            = 1.

*** Remove ordens que não sejam odometro/horimetro
      DELETE it_not WHERE u_status NE 'HOR'
                     AND  u_status NE 'HOD'.

*** Remove ordens encerradas e marcadas para eliminação
      DELETE it_not WHERE s_status CS 'MSEN'
                      OR  s_status CS 'MREL'.

      IF it_not IS NOT INITIAL.
        SORT it_not BY notifdate DESCENDING.
        READ TABLE it_not INTO wa_not INDEX 1.

*** Calcula tempo da primeira nota aberta para o equipamento
        CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
          EXPORTING
            date1    = wa_not-notifdate
            time1    = wa_not-notiftime
            date2    = sy-datum
            time2    = sy-uzeit
          IMPORTING
            datediff = lv_dias_aberto
          EXCEPTIONS
            OTHERS   = 1.

        MOVE lv_dias_aberto TO gv_dt_nota_aberto.
        SHIFT gv_dt_nota_aberto LEFT DELETING LEADING '0'.


        CASE wa_input-i_result_validacao_km_hr.
          WHEN 1.

            IF v_medato > v_medano.
              CLEAR lv_msg.
              IF gv_dt_nota_aberto IS INITIAL.
                lv_msg = 'Existe nota pendente de horimetro/odometro defeituoso para o equipamento informado no dia de hoje.'. "Deseja apontar Horas/Km?' ''.
              ELSE.
                CONCATENATE 'Existe nota pendente de horimetro/odometro defeituoso para o equipamento informado a ' gv_dt_nota_aberto 'dias. Não é possível fazer apontamento.' INTO lv_msg SEPARATED BY space.
              ENDIF.
              ls_return-message = lv_msg.
              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                          CHANGING et_return.
              p_erro = 'X'.
              EXIT.
            ENDIF.

          WHEN OTHERS.

            CASE wa_input-i_mot_inconsist_digi_km_hr.
              WHEN 2.
                IF v_medato >= v_medano.

                  v_medato = v_medano.
                  CLEAR _text_sort.
                  _text_sort = 'Hod/Hor Quebrado'.
                  CLEAR lv_msg.
                  CONCATENATE 'Não é permitido fazer apontamento de horimetro/odometro maior sem que seja concluído a nota aberta.' '' INTO lv_msg SEPARATED BY space.
                  ls_return-message = lv_msg.
                  PERFORM zf_inserir_mensagem USING 'W' 999 space space ls_return-message  space
                                              CHANGING et_return.
                ENDIF.

              WHEN OTHERS.

                DATA(_text_resul) = SWITCH #( wa_input-i_result_validacao_km_hr WHEN 2 THEN 'HR abaixo do HR do abastec. anterior'
                                                                          WHEN 3 THEN 'HR acima do limite previsto'
                                                                          WHEN 4 THEN 'KM abaixo do KM do abastec. anterior'
                                                                          WHEN 5 THEN 'KM acima do limite previsto'
                                            ).
                DATA(_text_mot) = SWITCH #( wa_input-i_mot_inconsist_digi_km_hr WHEN 1 THEN 'Nenhum'
                                                                                WHEN 3 THEN 'Hod/Hor Substituido'
                                                                                WHEN 4 THEN 'Conferido manualmente'
                                          ).

                ls_return-message = |{ _text_resul } { _text_mot }|.
                PERFORM zf_inserir_mensagem USING 'E' 999 space space ls_return-message  space
                                            CHANGING et_return.
                p_erro = 'X'.
                EXIT.
            ENDCASE.

        ENDCASE.

      ENDIF.
*
    ENDIF.  "Endif da Verificação se já houve Apontamento

    PERFORM  zf_test_bapi CHANGING p_erro.

    IF v_pointo IS INITIAL
*    OR V_MEDATO IS INITIAL.
      OR ( v_medato IS INITIAL AND
           lv_ja_apontou IS INITIAL ).

      ls_return-message = 'Medição não informada'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.
      p_erro = 'X'.
      EXIT.
    ELSE.
      IF v_dt_ponto IS INITIAL
      OR v_hr_ponto IS INITIAL.

        ls_return-message = 'Informar data e hora do ponto.'.
        PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                    CHANGING et_return.
        p_erro = 'X'.
        EXIT.
      ELSE.
        IF lv_erro_bapi IS INITIAL.

          IF lv_ja_apontou IS INITIAL.

            CASE wa_input-i_result_validacao_km_hr.
              WHEN 1.
                " Se não existir erro de apontamento cria documentos de medição
                READ TABLE it_dimpt INTO wa_dimpt WITH KEY atnam = vtpponto.
                IF sy-subrc = 0.
                  IF NOT v_medato IS INITIAL.
                    IF v_medato GE v_medano.
                      IF NOT vtpponto EQ 'ODOMETRO'.
*                    Validar Odometro
                        TRY.
                            CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
                              EXPORTING
                                date1            = wa_value-idate
                                time1            = wa_value-itime
                                date2            = v_dt_ponto
                                time2            = v_hr_ponto
                              IMPORTING
                                datediff         = difd
                                timediff         = difh
                              EXCEPTIONS
                                invalid_datetime = 1
                                OTHERS           = 2.

                            lv_hor = difd * 24.
                            lv_hor = difh + lv_hor.

                          CATCH cx_root.
                            ls_return-message = 'Erro ao converter data e hora do ponto'.
                            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                                        CHANGING et_return.
                            p_erro = 'X'.
                            EXIT.
                        ENDTRY.

                        lv_difer = v_medato - v_medano.

                        IF lv_difer > lv_hor.

                          ls_return-message = TEXT-091.
                          PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                                      CHANGING et_return.
                          p_erro = 'X'.
                          EXIT.
                        ENDIF.
                      ENDIF.
                      IF NOT line_exists( it_return[ type = 'E' ] ) AND p_erro IS INITIAL.
                        tl_filtro[] = it_saida2[].
                        DELETE tl_filtro WHERE code     IS INITIAL
                                           AND code_ref IS INITIAL
                                           AND motivo   IS INITIAL
                                           AND readg    IS INITIAL.

                        tl_oleo[] = it_saida1[].
                        DELETE tl_oleo WHERE code     IS INITIAL
                                         AND code_ref IS INITIAL
                                         AND motivo   IS INITIAL
                                         AND readg    IS INITIAL.

                        MOVE-CORRESPONDING wa_comb TO wl_comb.

                        IF tl_filtro[] IS NOT INITIAL OR
                          tl_oleo[] IS NOT INITIAL OR
                          wl_comb IS NOT INITIAL.
                          wa_status-status_int = 'E0001'.
                          PERFORM zf_save_cont USING v_medato.
                        ELSE.

                          ls_return-message = 'Não pode ser criado documento de medição sem fazer nenhum apontamento(Combustível, Lubrificante, Filtro)'.
                          PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                                      CHANGING et_return.

                          p_erro = 'X'.
                          EXIT.

                          REFRESH: it_saida1, it_saida2.

                        ENDIF.

                      ENDIF.

                    ELSE.

                      ls_return-message = TEXT-063.
                      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                                  CHANGING et_return.

                      p_erro = 'X'.
                      EXIT.
                    ENDIF.

                  ELSE.
                    ls_return-message = TEXT-058.
                    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                                CHANGING et_return.

                    p_erro = 'X'.
                    EXIT.
                  ENDIF.
                ENDIF.

              WHEN 2 OR 3 OR 4 OR 5.

                CASE wa_input-i_mot_inconsist_digi_km_hr.
                  WHEN 2.
                    IF vtpponto = 'ODOMETRO'.
                      v_erroo = 'X'.
                      CLEAR: v_erroh.
                    ELSE.
                      v_erroh = 'X'.
                      CLEAR: v_erroo.
                    ENDIF.

                    IF NOT v_dt_ponto > sy-datum.

                      IF NOT v_erroo IS INITIAL.

                        READ TABLE it_dimpt INTO wa_dimpt WITH KEY atnam = vtpponto.
                        IF sy-subrc IS INITIAL.

                          IF NOT v_medato IS INITIAL.
                            SELECT SINGLE zval
                            FROM ztparam
                            INTO lv_hod
                            WHERE param = 'ACONT'
                              AND const = 'HOD'.

                            CLEAR lv_difer.
                            lv_difer = v_medato - v_medano.
                            IF lv_difer > lv_hod.
                              CONCATENATE TEXT-016 '-' TEXT-081 INTO ls_return-message SEPARATED BY space.
                              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                              p_erro = abap_true.
                              EXIT.
                              IF lv_resp = 2.
                                CLEAR lv_resp.
                                CONCATENATE TEXT-016 TEXT-083 INTO ls_return-message SEPARATED BY space.
                                PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                                p_erro = abap_true.
                                EXIT.
                                IF lv_resp = 2.
                                  CLEAR lv_resp.
                                  CONCATENATE TEXT-016 TEXT-085 INTO ls_return-message SEPARATED BY space.
                                  PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                                  p_erro = abap_true.
                                  EXIT.
                                  IF lv_resp = 2.
                                    IF it_retmed1 IS INITIAL.
                                      wa_status-status_int = 'E0001'.
                                      PERFORM zf_save_cont USING v_medato.
                                    ENDIF.
                                  ELSE.
                                    CLEAR: wa_saida1, it_saida1[].
                                    CLEAR: wa_saida2, it_saida2[].
                                    CLEAR: wa_comb, v_erro, v_erroo, v_erroh,
                                           v_equip, wa_equi, v_equnr,
                                           v_pointo, v_itemo, v_medano, v_medato, v_unido,
                                           v_pointh, v_itemh, v_medanh, v_medath, v_unidh.
                                  ENDIF.
                                ELSE.
                                  CLEAR: wa_saida1, it_saida1[].
                                  CLEAR: wa_saida2, it_saida2[].
                                  CLEAR: wa_comb, v_erro, v_erroo, v_erroh,
                                         v_equip, wa_equi, v_equnr,
                                         v_pointo, v_itemo, v_medano, v_medato, v_unido,
                                         v_pointh, v_itemh, v_medanh, v_medath, v_unidh.
                                ENDIF.
                              ELSE.
                                CLEAR: wa_saida1, it_saida1[].
                                CLEAR: wa_saida2, it_saida2[].
                                CLEAR: wa_comb, v_erro, v_erroo, v_erroh,
                                       v_equip, wa_equi, v_equnr,
                                       v_pointo, v_itemo, v_medano, v_medato, v_unido,
                                       v_pointh, v_itemh, v_medanh, v_medath, v_unidh.
                              ENDIF.
                            ELSE.
                              IF NOT line_exists( it_return[ type = 'E' ] ).
                                wa_status-status_int = 'E0001'.
                                PERFORM zf_save_cont_erro USING v_medato 'Hod/Hor quebrado'.
                              ENDIF.
                            ENDIF.
                          ELSE.
                            ls_return-message = TEXT-058.
                            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                            p_erro = abap_true.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.

                      IF NOT v_erroh IS INITIAL.
                        READ TABLE it_dimpt INTO wa_dimpt WITH KEY atnam = vtpponto.
                        IF sy-subrc = 0.
                          IF NOT v_medato IS INITIAL.
                            IF v_medato >= v_medano.
                              TRY.
                                  CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
                                    EXPORTING
                                      date1            = wa_value-idate
                                      time1            = wa_value-itime
                                      date2            = v_dt_ponto
                                      time2            = v_hr_ponto
                                    IMPORTING
                                      datediff         = difd
                                      timediff         = difh
                                    EXCEPTIONS
                                      invalid_datetime = 1
                                      OTHERS           = 2.

                                  lv_hor = difd * 24.
                                  lv_hor = difh + lv_hor.

                                CATCH cx_root.
                                  ls_return-message = 'Erro ao converter'.
                                  PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                                  p_erro = abap_true.
                                  EXIT.
                              ENDTRY.
                              CLEAR lv_difer.
                              lv_difer = v_medato - v_medano.
                              IF lv_difer > lv_hor.
                                ls_return-message = TEXT-062.
                                PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                                p_erro = abap_true.
                                EXIT.
                              ELSE.
                                IF NOT line_exists( it_return[ type = 'E' ] ).
                                  wa_status-status_int = 'E0002'.
                                  PERFORM zf_save_cont_erro USING v_medato 'Hod/Hor quebrado'.
                                ENDIF.
                              ENDIF.
                            ELSE.
                              ls_return-message = TEXT-063.
                              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                              p_erro = abap_true.
                              EXIT.
                            ENDIF.
                          ELSE.
                            ls_return-message = TEXT-058.
                            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                            p_erro = abap_true.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ELSE.
                      ls_return-message = TEXT-061.
                      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                      p_erro = abap_true.
                      EXIT.
                    ENDIF.

                  WHEN 3. " Substituido

                    IF vtpponto = 'ODOMETRO'.
                      v_erroo = 'X'.
                      CLEAR: v_erroh.
                    ELSE.
                      v_erroh = 'X'.
                      CLEAR: v_erroo.
                    ENDIF.

                    IF NOT v_dt_ponto > sy-datum.
                      IF NOT v_erroo IS INITIAL.
                        READ TABLE it_dimpt INTO wa_dimpt WITH KEY atnam = vtpponto.
                        IF sy-subrc IS INITIAL.

                          IF NOT v_medato IS INITIAL.

                            SELECT SINGLE zval
                            FROM ztparam
                            INTO lv_hod
                            WHERE param = 'ACONT'
                              AND const = 'HOD'.

                            CLEAR lv_difer.

                            lv_difer = v_medato - v_medano.

                            IF lv_difer > lv_hod.
                              CONCATENATE TEXT-016 '-' TEXT-081 INTO ls_return-message SEPARATED BY space.
                              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                              p_erro = abap_true.
                              EXIT.
                              IF lv_resp = 2.
                                CLEAR lv_resp.
                                CONCATENATE TEXT-016 TEXT-083 INTO ls_return-message SEPARATED BY space.
                                PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                                p_erro = abap_true.
                                EXIT.
                                IF lv_resp = 2.
                                  CLEAR lv_resp.
                                  CONCATENATE TEXT-016 TEXT-085 INTO ls_return-message SEPARATED BY space.
                                  PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                                              CHANGING et_return.
                                  p_erro = abap_true.
                                  EXIT.
                                  IF lv_resp = 2.
                                    IF it_retmed1 IS INITIAL.
                                      wa_status-status_int = 'E0001'.
                                      PERFORM zf_save_cont USING v_medato.
                                    ENDIF.
                                  ELSE.
                                    CLEAR: wa_saida1, it_saida1[].
                                    CLEAR: wa_saida2, it_saida2[].
                                    CLEAR: wa_comb, v_erro, v_erroo, v_erroh,
                                           v_equip, wa_equi, v_equnr,
                                           v_pointo, v_itemo, v_medano, v_medato, v_unido,
                                           v_pointh, v_itemh, v_medanh, v_medath, v_unidh.
                                  ENDIF.
                                ELSE.
                                  CLEAR: wa_saida1, it_saida1[].
                                  CLEAR: wa_saida2, it_saida2[].
                                  CLEAR: wa_comb, v_erro, v_erroo, v_erroh,
                                         v_equip, wa_equi, v_equnr,
                                         v_pointo, v_itemo, v_medano, v_medato, v_unido,
                                         v_pointh, v_itemh, v_medanh, v_medath, v_unidh.
                                ENDIF.
                              ELSE.
                                CLEAR: wa_saida1, it_saida1[].
                                CLEAR: wa_saida2, it_saida2[].
                                CLEAR: wa_comb, v_erro, v_erroo, v_erroh,
                                       v_equip, wa_equi, v_equnr,
                                       v_pointo, v_itemo, v_medano, v_medato, v_unido,
                                       v_pointh, v_itemh, v_medanh, v_medath, v_unidh.
                              ENDIF.
                            ELSE.
                              IF NOT line_exists( it_return[ type = 'E' ] ).
                                wa_status-status_int = 'E0001'.
                                PERFORM zf_save_cont_erro USING v_medato 'Hod/Hor substituido'.
                              ENDIF.
                            ENDIF.
                          ELSE.
                            ls_return-message = TEXT-058.
                            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                            p_erro = abap_true.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.

                      IF NOT v_erroh IS INITIAL.
                        READ TABLE it_dimpt INTO wa_dimpt WITH KEY atnam = vtpponto.
                        IF sy-subrc = 0.
                          IF NOT v_medato IS INITIAL.

                            TRY.
                                CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
                                  EXPORTING
                                    date1            = wa_value-idate
                                    time1            = wa_value-itime
                                    date2            = v_dt_ponto
                                    time2            = v_hr_ponto
                                  IMPORTING
                                    datediff         = difd
                                    timediff         = difh
                                  EXCEPTIONS
                                    invalid_datetime = 1
                                    OTHERS           = 2.

                                lv_hor = difd * 24.
                                lv_hor = difh + lv_hor.

                              CATCH cx_root.
                                ls_return-message = 'Erro ao converter'.
                                PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                                p_erro = abap_true.
                                EXIT.
                            ENDTRY.

                            CLEAR lv_difer.
                            lv_difer = v_medato - v_medano.
                            IF lv_difer > lv_hor.
                              ls_return-message = TEXT-062.
                              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                              p_erro = abap_true.
                              EXIT.
                            ELSE.
                              IF NOT line_exists( it_return[ type = 'E' ] ).
                                wa_status-status_int = 'E0002'.
                                PERFORM zf_save_cont_erro USING v_medato 'Hod/Hor substituido'.
                              ENDIF.
                            ENDIF.
                          ELSE.
                            ls_return-message = TEXT-058.
                            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                            p_erro = abap_true.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ELSE.
                      ls_return-message = TEXT-061.
                      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
                      p_erro = abap_true.
                      EXIT.
                    ENDIF.
                  WHEN 1 OR 4.
                    ls_return-message = |Inconsistênçia nº. { wa_input-i_mot_inconsist_digi_km_hr } não foi Previsto!|.
                    PERFORM zf_inserir_mensagem USING c_e 999 space space space space CHANGING et_return.
                    p_erro = abap_true.
                  WHEN OTHERS.
                    ls_return-message = |Inconsistênçia nº. { wa_input-i_mot_inconsist_digi_km_hr } não encontrado!|.
                    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
                    p_erro = abap_true.
                    EXIT.
                ENDCASE.
              WHEN OTHERS.
                ls_return-message = |Resultado da Validação nº. { wa_input-i_result_validacao_km_hr } não encontrado!|.
                PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
                p_erro = abap_true.
                EXIT.
            ENDCASE.

          ENDIF. " Endif da verificação se já passou pelo Horimetro/Odometro

          IF p_erro IS INITIAL.
            IF lv_resp IS INITIAL OR
               lv_resp = 2.
*            Cria documento de medição de óleo
              DELETE it_saida1 WHERE code   IS INITIAL
                               AND code_ref IS INITIAL
                               AND posto    IS INITIAL
                               AND motivo   IS INITIAL
                               AND readg    IS INITIAL.

              IF NOT it_saida1[] IS INITIAL.
                IF NOT line_exists( it_return[ type = 'E' ] ).
                  PERFORM zf_save_lubri.
                ENDIF.
              ENDIF.

              " Cria documento de medição de filtro
              DELETE it_saida2 WHERE code     IS INITIAL
                                 AND code_ref IS INITIAL
                                 AND readg    IS INITIAL
                                 AND motivo   IS INITIAL.
              IF NOT it_saida2 IS INITIAL.
                IF NOT line_exists( it_return[ type = 'E' ] ).
                  PERFORM zf_save_filtro.
                ENDIF.
              ENDIF.

*            Cria medição de combustível
              IF NOT line_exists( it_return[ type = 'E' ] ).
                IF NOT wa_comb-comb IS INITIAL.
                  PERFORM zf_save_comb.
                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
  ELSE.
    IF NOT wa_comb-comb IS INITIAL.
      PERFORM zf_save_comb.
    ENDIF.
  ENDIF.

*  LOOP AT IT_RET INTO WA_RET.
*    WA_RETMED-MESSAGE = WA_RET-MESSAGE.
*    APPEND WA_RETMED TO IT_RETMED.
*    CLEAR WA_RETMED.
*  ENDLOOP.

  IF p_erro IS INITIAL.
*** Grava log
    lv_hora       = sy-uzeit.
    lv_data       = sy-datum.

    LOOP AT it_return INTO wa_return WHERE type = 'S'.
      CLEAR wa_log.
      PERFORM gera_id CHANGING wa_log-id.
      wa_log-hora       = lv_hora.
      wa_log-data       = lv_data.
      wa_log-usuario    = sy-uname.
      wa_log-mensagem   = wa_return-message.

      INSERT INTO zlogapont VALUES wa_log.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM it_log COMPARING ALL FIELDS.

    LOOP AT it_log INTO wa_log.
      PERFORM gera_id CHANGING wa_log-id.
      wa_log-hora       = lv_hora.
      wa_log-data       = lv_data.
      wa_log-usuario    = sy-uname.

      INSERT INTO zlogapont VALUES wa_log.
    ENDLOOP.

    PERFORM gera_id CHANGING wa_log-id.
    wa_log-hora       = lv_hora.
    wa_log-data       = lv_data.
    wa_log-usuario    = sy-uname.
    CONCATENATE 'Apontamento para o equipamento' v_equip '.' INTO wa_log-mensagem SEPARATED BY space.

    INSERT INTO zlogapont VALUES wa_log.

    PERFORM gera_id CHANGING wa_log-id.
    wa_log-hora       = lv_hora.
    wa_log-data       = lv_data.
    wa_log-usuario    = sy-uname.
    CONCATENATE v_dt_ponto+6(2) '.' v_dt_ponto+4(2) '.' v_dt_ponto+0(4) '/' v_hr_ponto+0(2)':' v_hr_ponto+2(2) '.' INTO wa_log-mensagem.
    CONCATENATE 'Apontamento informado para ' wa_log-mensagem INTO wa_log-mensagem SEPARATED BY space.

    INSERT INTO zlogapont VALUES wa_log.
*** Fim Grava log

    CLEAR: wa_saida1, it_saida1[].
    CLEAR: wa_saida2, it_saida2[].
    CLEAR: wa_comb, v_erro, v_erroo, v_erroh, v_dt_ponto,
           v_hr_ponto, v_equip, wa_equi, v_equnr,
           v_hr_pto, v_pointo, v_itemo, v_medano, v_medato,
           v_unido, v_pointh, v_itemh, v_medanh, v_medath,
           v_unidh, it_log[], v_nbaixa, v_n_form.
  ENDIF.
*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ABRIR_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_abrir_trans .
  DATA: wl_lubr TYPE ty_zftpme_lubrificante,
        wl_filt TYPE ty_zftpme_filtros.

  DATA: BEGIN OF tl_temp OCCURS 0,
          tipo    TYPE c LENGTH 4,
          ordem   TYPE zftpme_lubrificante-ordem,
          sistema TYPE zftpme_lubrificante-sistema,
          plano   TYPE rmipm-warpl,
        END OF tl_temp.

  CLEAR: wl_lubr, wl_filt, tl_temp[].

  IF ok_code = 'EXT_BAIXAS'.
    PERFORM zf_shdb USING 'MBST' '' '' ''."wa_equi-aufnr ''.
  ELSEIF ok_code ='HR_TRAB'.

    LOOP AT it_saida1 INTO wl_lubr WHERE check = 'X'.
      tl_temp-tipo  = 'IW41'.
      tl_temp-ordem = wl_lubr-ordem.
      APPEND tl_temp.
    ENDLOOP.

    LOOP AT it_saida2 INTO wl_filt WHERE check = 'X'.
      tl_temp-tipo  = 'IW41'.
      tl_temp-ordem = wl_filt-ordem.
      APPEND tl_temp.
    ENDLOOP.

    SORT tl_temp BY ordem.

*    DELETE ADJACENT DUPLICATES FROM TL_TEMP COMPARING ALL FIELDS.

    LOOP AT tl_temp.
      PERFORM zf_shdb USING tl_temp-tipo tl_temp-ordem '' ''."wa_equi-aufnr ''.
    ENDLOOP.

    IF tl_temp IS INITIAL.
      ls_return-message = 'Selecione uma ordem válida para prosseguir.'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space space  space
                                  CHANGING et_return.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERA_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_LOG_ID  text
*----------------------------------------------------------------------*
FORM gera_id CHANGING p_id TYPE i.
  SELECT COUNT(*) INTO p_id FROM zlogapont.   ADD 1 TO p_id.
*  SELECT ID
*    INTO P_ID
*    FROM ZLOGAPONT.
*  ENDSELECT.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE_COMB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_save_comb .

  DATA: ls_header          TYPE bapi2017_gm_head_01,
        ls_code            TYPE bapi2017_gm_code,
        ls_item            TYPE bapi2017_gm_item_create,
        lt_item            TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_return          TYPE STANDARD TABLE OF bapiret2,
        lw_return          TYPE bapiret2,
        ls_testrun         TYPE STANDARD TABLE OF bapi2017_gm_gen,
        lw_testrun         TYPE bapi2017_gm_gen,
*          LS_RETURN          TYPE BAPIRET2,
        lv_valor           TYPE ztparam-zval,
        lv_measurement_doc TYPE imrg-mdocm,
        ls_doc_complete    TYPE imrg,
        ls_notification    TYPE qmel-qmnum,
        lv_point           TYPE imrg-point,
        lv_value           TYPE rimr0-recdc,
        lv_matnr           TYPE matnr,
        lv_mater           TYPE bapi2017_gm_head_ret-mat_doc,
        lv_mater2          TYPE mara-matnr,
        lv_matkl           TYPE mara-matkl,
        lv_errsubrc        TYPE c LENGTH 50,
        wa_zpmr0009        TYPE zpmr0009.

  DATA: vg_matnr TYPE char18.

*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      PERCENTAGE = SY-INDEX
*      TEXT       = 'Salvando dados de abastecimento...'.

  CLEAR: wa_retmed, lt_return, lv_value, p_erro.

  MOVE wa_comb-quant TO lv_value.
  CONDENSE lv_value NO-GAPS.
  REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.

*=======================================================================================
  "Verifica se tem doc hr/km gerado para continual e fazer o combustivel.
*  IF T_DOC_MED IS INITIAL.
*
*    LS_RETURN-MESSAGE = 'Erro ao criar doc mediçao Hr/Km'.
*    PERFORM ZF_INSERIR_MENSAGEM USING C_E 999 SPACE SPACE LS_RETURN-MESSAGE  SPACE
*                                    CHANGING ET_RETURN.
*    P_ERRO = 'X'.
*    EXIT.
*  ENDIF.
*  =====================================================================================

  IF NOT wa_equi-daufn IS INITIAL.

    SELECT SINGLE zval
    FROM ztparam
    INTO lv_valor
    WHERE param = 'CONT'
      AND const = 'COMB'.



    IF wa_equi-eqtyp NE 'M'.


      IF v_n_form IS NOT INITIAL.
        CONCATENATE 'Formulário N.' v_n_form INTO string_form SEPARATED BY space. "/Modificação CS2016001593
      ENDIF.

      " Grava Apontamento de Consumo
      CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
        EXPORTING
          measurement_point    = v_pt_comb
          reading_date         = v_dt_ponto
          reading_time         = v_hr_ponto
          reader               = lv_reader
          origin_indicator     = 'A'
          recorded_value       = lv_value
          difference_reading   = abap_true
          prepare_update       = 'X'
          commit_work          = 'X'
          wait_after_commit    = 'X'
          short_text           = string_form                          "/Modificação CS2016001593
          user_data            = imrg_usr
        IMPORTING
          measurement_document = lv_measurement_doc
          complete_document    = ls_doc_complete
          notification         = ls_notification
        EXCEPTIONS
          no_authority         = 1
          point_not_found      = 2
          index_not_unique     = 3
          type_not_found       = 4
          point_locked         = 5
          point_inactive       = 6
          timestamp_in_future  = 7
          timestamp_duprec     = 8
          unit_unfit           = 9
          value_not_fltp       = 10
          value_overflow       = 11
          value_unfit          = 12
          value_missing        = 13
          code_not_found       = 14
          notif_type_not_found = 15
          notif_prio_not_found = 16
          notif_gener_problem  = 17
          update_failed        = 18
          invalid_time         = 19
          invalid_date         = 20
          OTHERS               = 21.


      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            lv_errsubrc = TEXT-065.
          WHEN 2.
            lv_errsubrc = TEXT-066.
          WHEN 3.
            lv_errsubrc = TEXT-067.
          WHEN 4.
            lv_errsubrc = TEXT-068.
          WHEN 5.
            lv_errsubrc = TEXT-069.
          WHEN 6.
            lv_errsubrc = TEXT-070.
          WHEN 7.
            lv_errsubrc = TEXT-071.
          WHEN 8.
            lv_errsubrc = TEXT-072.
          WHEN 9.
            lv_errsubrc = TEXT-073.
          WHEN 10.
            lv_errsubrc = TEXT-074.
          WHEN 11.
            lv_errsubrc = TEXT-074.
          WHEN 12.
            lv_errsubrc = TEXT-074.
          WHEN 13.
            lv_errsubrc = TEXT-075.
          WHEN 14.
            lv_errsubrc = TEXT-076.
          WHEN 19.
            lv_errsubrc = TEXT-077.
          WHEN 20.
            lv_errsubrc = TEXT-072.
          WHEN OTHERS.
            lv_errsubrc = TEXT-098.
        ENDCASE.

*      WA_RETMED-TIPO = 'ECOMB'.
        CONCATENATE TEXT-052 TEXT-064 lv_errsubrc
               INTO ls_return-message SEPARATED BY space.

        PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                    CHANGING et_return.

*      APPEND WA_RETMED TO IT_RETMED.
*      CLEAR WA_RETMED.

*      WA_RETMED1-TIPO = 'E'.
*      APPEND WA_RETMED1 TO IT_RETMED1.
*      CLEAR WA_RETMED1.
*
        p_erro = 'X'.
        EXIT.
      ELSE.
*      CONCATENATE TEXT-041 LV_MEASUREMENT_DOC TEXT-042
*                   INTO LS_RETURN-MESSAGE SEPARATED BY SPACE.

*      PERFORM ZF_INSERIR_MENSAGEM USING C_S 999 SPACE SPACE SPACE  SPACE
*                                  CHANGING ET_RETURN.

*      APPEND WA_RETMED TO IT_RETMED.
*      CLEAR WA_RETMED.
        APPEND VALUE #( mdocm =  lv_measurement_doc ) TO t_doc_med.
        ls_header-pstng_date = v_dt_ponto.
        ls_header-doc_date   = v_dt_ponto.
        ls_header-ref_doc_no = wa_equi-daufn.

        ls_code-gm_code      = '03'.


*---> 28/06/2023 - migração s4 - lo
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = wa_comb-comb
*          IMPORTING
*            output = ls_item-material.

*        DATA(v_mat) = |{ wa_comb-comb ALPHA = IN  }|.
*        DATA(v_len) = strlen( v_mat ).
*
*        IF v_len > 18.
*          ls_item-material_long = v_mat.
*        ELSE.
*          ls_item-material      = v_mat.
*        ENDIF.
*
*        CLEAR: v_mat, v_len.
*<--- 28/06/2023 - migração s4 - lo

*<--- 28/06/2023 - migração s4 - lo
        SELECT SINGLE matkl
        FROM  mara
        INTO lv_matkl
        WHERE matnr = ls_item-material.

        CLEAR: vg_matnr_aux.
        FREE: rg_matnr.

        IF wa_comb-comb IS NOT INITIAL.

          vg_matnr_aux = wa_comb-comb.
          vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
          ls_item-material = vg_matnr_aux.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ wa_comb-comb ALPHA = IN }| ) TO rg_matnr.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ ls_item-material }| ) TO rg_matnr.

          SELECT SINGLE matkl
          FROM  mara
          INTO lv_matkl
          WHERE matnr IN rg_matnr.
        ENDIF.
        "*<--- 28/06/2023 - Migração S4 - LO

        SELECT SINGLE saknr
        FROM zmmt0039
        INTO ls_item-gl_account
        WHERE matkl = lv_matkl.


        ls_item-plant        = wa_equi-swerk.
        ls_item-move_type    = '261'.
        ls_item-entry_qnt    = wa_comb-quant.
        ls_item-orderid      = wa_equi-daufn.

        SELECT SINGLE storage
        FROM t370fld_stn
        INTO ls_item-stge_loc
        WHERE station = wa_comb-posto.

        wa_input-ib_ordem = |{ wa_input-ib_ordem ALPHA = IN }|.

        IF wa_input-ib_ordem IS NOT INITIAL.
          SELECT SINGLE vornr
            FROM afvc AS a
           INNER JOIN afko AS b ON a~aufpl = b~aufpl
            INTO ls_item-activity
           WHERE b~aufnr = wa_input-ib_ordem.
        ELSE.
          SELECT SINGLE vornr
          FROM afvc AS a
         INNER JOIN afko AS b ON a~aufpl = b~aufpl
          INTO ls_item-activity
         WHERE b~aufnr = ls_item-orderid.
        ENDIF.

        APPEND ls_item TO lt_item.
        CLEAR ls_item.

        lw_testrun-testrun = ' '.
        APPEND lw_testrun TO ls_testrun.

        CLEAR lv_mater.
        IF v_nbaixa IS INITIAL.
          "*---> 28/06/2023 - Migração S4 - LO
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = ls_header
              goodsmvt_code    = ls_code
            IMPORTING
              materialdocument = lv_mater
            TABLES
              goodsmvt_item    = lt_item
              return           = lt_return.

          CLEAR: ls_header, ls_code, lt_item[].

*          IF SY-SUBRC = 0 AND LT_RETURN IS INITIAL.
          IF lv_mater IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            wa_input-doc_material = lv_mater.

            CONCATENATE TEXT-053 lv_mater TEXT-042 INTO ls_return-message SEPARATED BY space.

            PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                        CHANGING et_return.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            LOOP AT  lt_return INTO lw_return.
              PERFORM zf_ins_mensagem USING c_e lw_return-number lw_return-id space space lw_return-message space CHANGING et_return.
              p_erro = 'X'.

********      Estorna documento de medição de horimetro/Odometro.
              PERFORM estor_documento_medicao.
              EXIT.
            ENDLOOP.
          ENDIF.

        ENDIF.

        "Grava Local de Abastecimento na tabela de apoio ZPMR0009
        IF wa_comb-local IS NOT INITIAL.
* verifica a existência da combinação Veículo X Operação
          DATA lv_code TYPE ztpm_vei_op_saaf-code.
          lv_code = |{ lv_operacao ALPHA = IN }|.

          CLEAR: wa_zpmr0009.

          SELECT SINGLE kurztext
            INTO wa_zpmr0009-kurztext
            FROM ztpm_vei_op_saaf
            WHERE code EQ lv_code
              AND eqart EQ wa_equi-eqart.

*        SELECT KURZTEXT INTO WA_ZPMR0009-KURZTEXT
*          FROM ZTPM_VEI_OP_SAAF UP TO 1 ROWS
*          WHERE CODE EQ LV_OPERACAO
*            AND EQART EQ WA_EQUI-EQART.
*        ENDSELECT.
          IF NOT sy-subrc IS INITIAL.
* Não existe a combinação esperada para Operação e Code
*          WA_RETMED-TIPO = 'ECOMB'.
            ls_return-message = TEXT-097.

            PERFORM zf_inserir_mensagem USING c_e 999 space space space  space
                                        CHANGING et_return.
*          APPEND WA_RETMED TO IT_RETMED.
*          CLEAR WA_RETMED.
*
*          WA_RETMED1-TIPO = 'E'.
*          APPEND WA_RETMED1 TO IT_RETMED1.
*          CLEAR WA_RETMED1.

            p_erro = 'X'.
          ELSE.
            wa_zpmr0009-mdocm = lv_measurement_doc.
            wa_zpmr0009-tplnr = wa_comb-local.
            wa_zpmr0009-fltyp = 'C'.
            wa_zpmr0009-pernr_mot = wa_input-s_cod_motorista.
            wa_zpmr0009-operacao = lv_operacao.
            MODIFY zpmr0009 FROM wa_zpmr0009.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
*      CONCATENATE TEXT-041 LV_MEASUREMENT_DOC TEXT-042
*                   INTO LS_RETURN-MESSAGE SEPARATED BY SPACE.

*      PERFORM ZF_INSERIR_MENSAGEM USING C_S 999 SPACE SPACE SPACE  SPACE
*                                  CHANGING ET_RETURN.

*      APPEND WA_RETMED TO IT_RETMED.
*      CLEAR WA_RETMED.

      ls_header-pstng_date = v_dt_ponto.
      ls_header-doc_date   = v_dt_ponto.
      ls_header-ref_doc_no = wa_equi-daufn.


      ls_code-gm_code      = '03'.

      "*---> 28/06/2023 - Migração S4 - LO
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = wa_comb-comb
*        IMPORTING
*          output = ls_item-material.
*
*      DATA(v_mat2) = |{ wa_comb-comb ALPHA = IN  }|.
*      DATA(v_len2) = strlen( v_mat2 ).
*
*      IF v_len2 > 18.
*        ls_item-material_long = v_mat2.
*      ELSE.
*        ls_item-material      = v_mat2.
*      ENDIF.
*      CLEAR: v_mat2, v_len2.
      "*---> 28/06/2023 - Migração S4 - LO

      "*---> 28/06/2023 - Migração S4 - LO
*      SELECT SINGLE matkl
*      FROM  mara
*      INTO lv_matkl
*      WHERE matnr = ls_item-material.
*      IF ls_item-material IS INITIAL .

      CLEAR: vg_matnr_aux.
      FREE: rg_matnr.

      IF wa_comb-comb  IS NOT INITIAL.

        vg_matnr_aux = wa_comb-comb.
        vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
        ls_item-material = vg_matnr_aux.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ wa_comb-comb  ALPHA = IN }| ) TO rg_matnr.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ ls_item-material }| ) TO rg_matnr.

      ENDIF.

      SELECT SINGLE matkl
      FROM  mara
      INTO lv_matkl
      WHERE matnr IN rg_matnr.
*      ELSE.
*
*        SELECT SINGLE matkl
*        FROM  mara
*        INTO lv_matkl
*        WHERE matnr = ls_item-material.
*      ENDIF.
      "*---> 28/06/2023 - Migração S4 - LO

      SELECT SINGLE saknr
      FROM zmmt0039
      INTO ls_item-gl_account
      WHERE matkl = lv_matkl.


      ls_item-plant        = wa_equi-swerk.
      ls_item-move_type    = '261'.
      ls_item-entry_qnt    = wa_comb-quant.
      ls_item-orderid      = wa_equi-daufn.

      SELECT SINGLE storage
      FROM t370fld_stn
      INTO ls_item-stge_loc
      WHERE station = wa_comb-posto.

      wa_input-ib_ordem = |{ wa_input-ib_ordem ALPHA = IN }|.

      IF wa_input-ib_ordem IS NOT INITIAL.
        SELECT SINGLE vornr
          FROM afvc AS a
         INNER JOIN afko AS b ON a~aufpl = b~aufpl
          INTO ls_item-activity
         WHERE b~aufnr = wa_input-ib_ordem.
      ELSE.
        SELECT SINGLE vornr
        FROM afvc AS a
       INNER JOIN afko AS b ON a~aufpl = b~aufpl
        INTO ls_item-activity
       WHERE b~aufnr = ls_item-orderid.
      ENDIF.

      APPEND ls_item TO lt_item.
      CLEAR ls_item.

*      LW_TESTRUN = SPACE.
*      APPEND LW_TESTRUN TO LS_TESTRUN.

      CLEAR lv_mater.
      IF v_nbaixa IS INITIAL.

        "*---> 28/06/2023 - Migração S4 - LO
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            goodsmvt_header  = ls_header
            goodsmvt_code    = ls_code
          IMPORTING
            materialdocument = lv_mater
          TABLES
            goodsmvt_item    = lt_item
            return           = lt_return.

        CLEAR: ls_header, ls_code, lt_item[].

*        IF SY-SUBRC = 0 AND LT_RETURN IS INITIAL.

        IF lv_mater IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          wa_input-doc_material = lv_mater.

          CONCATENATE TEXT-053 lv_mater TEXT-042 INTO ls_return-message SEPARATED BY space.

          PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                      CHANGING et_return.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          LOOP AT  lt_return INTO lw_return.
            PERFORM zf_ins_mensagem USING c_e lw_return-number lw_return-id space space lw_return-message space CHANGING et_return.
            p_erro = 'X'.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.

      "Grava Local de Abastecimento na tabela de apoio ZPMR0009
      IF wa_comb-local IS NOT INITIAL.
* verifica a existência da combinação Veículo X Operação
        DATA lz_code TYPE ztpm_vei_op_saaf-code.
        lv_code = |{ lv_operacao ALPHA = IN }|.

        CLEAR: wa_zpmr0009.

        SELECT SINGLE kurztext
          INTO wa_zpmr0009-kurztext
          FROM ztpm_vei_op_saaf
          WHERE code EQ lz_code
            AND eqart EQ wa_equi-eqart.

*        SELECT KURZTEXT INTO WA_ZPMR0009-KURZTEXT
*          FROM ZTPM_VEI_OP_SAAF UP TO 1 ROWS
*          WHERE CODE EQ LV_OPERACAO
*            AND EQART EQ WA_EQUI-EQART.
*        ENDSELECT.
        IF NOT sy-subrc IS INITIAL.
* Não existe a combinação esperada para Operação e Code
*          WA_RETMED-TIPO = 'ECOMB'.
          ls_return-message = TEXT-097.

          PERFORM zf_inserir_mensagem USING c_e 999 space space space  space
                                      CHANGING et_return.
*          APPEND WA_RETMED TO IT_RETMED.
*          CLEAR WA_RETMED.
*
*          WA_RETMED1-TIPO = 'E'.
*          APPEND WA_RETMED1 TO IT_RETMED1.
*          CLEAR WA_RETMED1.
          p_erro = 'X'.
        ELSE.
          wa_zpmr0009-mdocm = lv_measurement_doc.
          wa_zpmr0009-tplnr = wa_comb-local.
          wa_zpmr0009-fltyp = 'C'.
          wa_zpmr0009-pernr_mot = wa_input-s_cod_motorista.
          wa_zpmr0009-operacao = lv_operacao.
          MODIFY zpmr0009 FROM wa_zpmr0009.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.

    ls_return-message = TEXT-047.

    PERFORM zf_inserir_mensagem USING c_e 999 space space space  space
                                CHANGING et_return.

*    WA_RETMED-TIPO = 'ECOMB'.
*    WA_RETMED-MESSAGE = TEXT-047.
*    APPEND WA_RETMED TO IT_RETMED.
*    CLEAR WA_RETMED.

*    WA_RETMED1-TIPO = 'E'.
**    APPEND WA_RETMED1 TO IT_RETMED1.
*    CLEAR WA_RETMED1.

    p_erro = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_MEDATO  text
*----------------------------------------------------------------------*
FORM zf_save_cont_erro USING p_value p_text.

  DATA: lv_resp     TYPE c LENGTH 1,
        lv_docu     TYPE imrg-mdocm,
        lv_nota     TYPE qmel-qmnum,
        lv_msg      TYPE c LENGTH 100,
        lt_return   TYPE STANDARD TABLE OF bapiret2,
        lv_value    TYPE rimr0-recdc,
        lv_val      TYPE ztparam-zval,
        lv_tpnota   TYPE qmel-qmart,
        lv_errsubrc TYPE c LENGTH 50,
        lv_point    TYPE diimpt-point.

  CLEAR: wa_retmed, lv_value, lv_errsubrc.

  lv_value = p_value.

  CASE wa_input-i_mot_inconsist_digi_km_hr.
    WHEN 02.
      IF NOT it_not[] IS INITIAL.

        REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
        CONDENSE lv_value NO-GAPS.

        MOVE v_pointo TO lv_point.

        CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
          EXPORTING
            measurement_point    = lv_point
            secondary_index      = ' '
            reading_date         = v_dt_ponto
            reading_time         = v_hr_ponto
            short_text           = p_text
            reader               = lv_reader
            origin_indicator     = ' ' "'A'
            reading_after_action = ' '
            recorded_value       = lv_value
            recorded_unit        = ' '
            difference_reading   = ' '
            code_version         = ' '
            "USER_DATA            = ' '
            check_custom_duprec  = ' '
            with_dialog_screen   = ' '
            prepare_update       = 'X'
            commit_work          = 'X'
            wait_after_commit    = 'X'
            create_notification  = ' '
            notification_type    = ' '
            notification_prio    = ' '
          IMPORTING
            measurement_document = lv_docu
            notification         = lv_nota
          EXCEPTIONS
            no_authority         = 1
            point_not_found      = 2
            index_not_unique     = 3
            type_not_found       = 4
            point_locked         = 5
            point_inactive       = 6
            timestamp_in_future  = 7
            timestamp_duprec     = 8
            unit_unfit           = 9
            value_not_fltp       = 10
            value_overflow       = 11
            value_unfit          = 12
            value_missing        = 13
            code_not_found       = 14
            notif_type_not_found = 15
            notif_prio_not_found = 16
            notif_gener_problem  = 17
            update_failed        = 18
            invalid_time         = 19
            invalid_date         = 20
            OTHERS               = 21.


        IF sy-subrc <> 0.
          lv_errsubrc = SWITCH #( sy-subrc
                                  WHEN 1  THEN TEXT-065
                                  WHEN 2  THEN TEXT-066
                                  WHEN 3  THEN TEXT-067
                                  WHEN 4  THEN TEXT-068
                                  WHEN 5  THEN TEXT-069
                                  WHEN 6  THEN TEXT-070
                                  WHEN 7  THEN TEXT-071
                                  WHEN 8  THEN TEXT-072
                                  WHEN 9  THEN TEXT-073
                                  WHEN 10 THEN TEXT-074
                                  WHEN 11 THEN TEXT-074
                                  WHEN 12 THEN TEXT-074
                                  WHEN 13 THEN TEXT-075
                                  WHEN 14 THEN TEXT-076
                                  WHEN 19 THEN TEXT-077
                                  WHEN 20 THEN TEXT-072
                                  ).
          IF lv_errsubrc IS INITIAL.
            lv_errsubrc = TEXT-098.
          ENDIF.
          ls_return-message = |{ TEXT-044 } { wa_dimpt-pttxt } { TEXT-054 } { TEXT-064 } { lv_errsubrc } |.
          PERFORM zf_inserir_mensagem USING c_e 999 space space space  space CHANGING et_return.
          p_erro = abap_true.
          EXIT.
        ELSE.
          ls_return-message = |{ TEXT-041 } { lv_docu ALPHA = OUT } { TEXT-055 } { wa_dimpt-pttxt } { TEXT-042 }|.
          PERFORM zf_inserir_mensagem USING c_s 0 space space space  space CHANGING et_return.
          APPEND VALUE #( mdocm =  lv_docu ) TO t_doc_med.
        ENDIF.

      ELSE.

        SELECT SINGLE zval
          FROM ztparam
            INTO lv_val
              WHERE param EQ 'TP_NOTA'
                AND const EQ 'HOR_HOD'.

        lv_tpnota = COND #( WHEN sy-subrc IS INITIAL THEN lv_val ).

        REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
        CONDENSE lv_value NO-GAPS.

        CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
          EXPORTING
            measurement_point    = wa_dimpt-point
            secondary_index      = ' '
            reading_date         = v_dt_ponto
            reading_time         = v_hr_ponto
            short_text           = p_text
            reader               = sy-uname
            origin_indicator     = 'A'
            reading_after_action = ' '
            recorded_value       = lv_value
            recorded_unit        = ' '
            difference_reading   = ' '
            code_version         = ' '
            "USER_DATA            = ' '
            check_custom_duprec  = ' '
            with_dialog_screen   = ' '
            prepare_update       = ' '
            commit_work          = ' '
            wait_after_commit    = ' '
            create_notification  = 'X'
            notification_type    = lv_tpnota
            notification_prio    = '1'
          IMPORTING
            measurement_document = lv_docu
            notification         = lv_nota
          EXCEPTIONS
            no_authority         = 1
            point_not_found      = 2
            index_not_unique     = 3
            type_not_found       = 4
            point_locked         = 5
            point_inactive       = 6
            timestamp_in_future  = 7
            timestamp_duprec     = 8
            unit_unfit           = 9
            value_not_fltp       = 10
            value_overflow       = 11
            value_unfit          = 12
            value_missing        = 13
            code_not_found       = 14
            notif_type_not_found = 15
            notif_prio_not_found = 16
            notif_gener_problem  = 17
            update_failed        = 18
            invalid_time         = 19
            invalid_date         = 20
            OTHERS               = 21.

        IF sy-subrc IS INITIAL.

          ls_return-message = |{ TEXT-041 } { lv_docu } { TEXT-042 } { TEXT-043 } { lv_nota } { TEXT-060 } { wa_dimpt-pttxt }|.
          PERFORM zf_inserir_mensagem USING c_s 0 space space space  space CHANGING et_return.

          CALL FUNCTION 'BAPI_ALM_NOTIF_CHANGEUSRSTAT'
            EXPORTING
              number     = lv_nota
              usr_status = wa_status
            TABLES
              return     = lt_return.

          IF NOT line_exists( lt_return[ type = 'E' ] ).
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
          ENDIF.

          APPEND VALUE #( mdocm =  lv_docu ) TO t_doc_med.
        ELSE.
          lv_errsubrc = SWITCH #( sy-subrc
                                            WHEN 1  THEN TEXT-065
                                            WHEN 2  THEN TEXT-066
                                            WHEN 3  THEN TEXT-067
                                            WHEN 4  THEN TEXT-068
                                            WHEN 5  THEN TEXT-069
                                            WHEN 6  THEN TEXT-070
                                            WHEN 7  THEN TEXT-071
                                            WHEN 8  THEN TEXT-072
                                            WHEN 9  THEN TEXT-073
                                            WHEN 10 THEN TEXT-074
                                            WHEN 11 THEN TEXT-074
                                            WHEN 12 THEN TEXT-074
                                            WHEN 13 THEN TEXT-075
                                            WHEN 14 THEN TEXT-076
                                            WHEN 19 THEN TEXT-077
                                            WHEN 20 THEN TEXT-072
                                 ).

          IF lv_errsubrc IS INITIAL.
            lv_errsubrc = TEXT-098.
          ENDIF.

          ls_return-message = |{ TEXT-044 } { wa_dimpt-pttxt } { TEXT-054 } { TEXT-063 } { lv_errsubrc }|.
          PERFORM zf_inserir_mensagem USING c_e 999 space space space  space CHANGING et_return.
          p_erro = abap_true.
          EXIT.
        ENDIF.
      ENDIF.

    WHEN 03.

      REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
      CONDENSE lv_value NO-GAPS.

      MOVE v_pointo TO lv_point.

      CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
        EXPORTING
          measurement_point    = lv_point
          secondary_index      = ' '
          reading_date         = v_dt_ponto
          reading_time         = v_hr_ponto
          short_text           = p_text
          reader               = lv_reader
          origin_indicator     = ' '
          reading_after_action = ' '
          recorded_value       = lv_value
          recorded_unit        = ' '
          difference_reading   = ' '
          code_version         = ' '
          "USER_DATA            = ' '
          check_custom_duprec  = ' '
          with_dialog_screen   = ' '
          prepare_update       = 'X'
          commit_work          = 'X'
          wait_after_commit    = 'X'
          create_notification  = ' '
          notification_type    = ' '
          notification_prio    = ' '
        IMPORTING
          measurement_document = lv_docu
          notification         = lv_nota
        EXCEPTIONS
          no_authority         = 1
          point_not_found      = 2
          index_not_unique     = 3
          type_not_found       = 4
          point_locked         = 5
          point_inactive       = 6
          timestamp_in_future  = 7
          timestamp_duprec     = 8
          unit_unfit           = 9
          value_not_fltp       = 10
          value_overflow       = 11
          value_unfit          = 12
          value_missing        = 13
          code_not_found       = 14
          notif_type_not_found = 15
          notif_prio_not_found = 16
          notif_gener_problem  = 17
          update_failed        = 18
          invalid_time         = 19
          invalid_date         = 20
          OTHERS               = 21.

      IF sy-subrc <> 0.
        lv_errsubrc = SWITCH #( sy-subrc
                                WHEN 1  THEN TEXT-065
                                WHEN 2  THEN TEXT-066
                                WHEN 3  THEN TEXT-067
                                WHEN 4  THEN TEXT-068
                                WHEN 5  THEN TEXT-069
                                WHEN 6  THEN TEXT-070
                                WHEN 7  THEN TEXT-071
                                WHEN 8  THEN TEXT-072
                                WHEN 9  THEN TEXT-073
                                WHEN 10 THEN TEXT-074
                                WHEN 11 THEN TEXT-074
                                WHEN 12 THEN TEXT-074
                                WHEN 13 THEN TEXT-075
                                WHEN 14 THEN TEXT-076
                                WHEN 19 THEN TEXT-077
                                WHEN 20 THEN TEXT-072
                     ).

        IF lv_errsubrc IS INITIAL.
          lv_errsubrc = TEXT-098.
        ENDIF.

        ls_return-message = |{ TEXT-044 } { wa_dimpt-pttxt } { TEXT-054 } { TEXT-064 } { lv_errsubrc } |.
        PERFORM zf_inserir_mensagem USING c_e 999 space space space  space CHANGING et_return.
        p_erro = abap_true.
        EXIT.
      ELSE.
        ls_return-message = |{ TEXT-041 } { lv_docu ALPHA = OUT } { TEXT-055 } { wa_dimpt-pttxt } { TEXT-042 }|.
        PERFORM zf_inserir_mensagem USING c_s 0 space space space  space CHANGING et_return.

        APPEND VALUE #( mdocm =  lv_docu ) TO t_doc_med.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TEST_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_ERRO_BAPI  text
*----------------------------------------------------------------------*
FORM zf_test_bapi  CHANGING p_erro.

  DATA: ls_header TYPE bapi2017_gm_head_01,
        ls_code   TYPE bapi2017_gm_code,
        ls_item   TYPE bapi2017_gm_item_create,
        lt_item   TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_return TYPE STANDARD TABLE OF bapiret2,
*        LS_RETURN TYPE BAPIRET2,
        lv_matkl  TYPE mara-matkl,
        lv_mater  TYPE bapi2017_gm_head_ret-mat_doc.

  DATA: lt_lubrificantes TYPE STANDARD TABLE OF ty_zftpme_lubrificante, "ZFTPME_LUBRIFICANTE,
        ls_lubrificantes TYPE ty_zftpme_lubrificante, "ZFTPME_LUBRIFICANTE,
        lt_filtros       TYPE STANDARD TABLE OF zftpme_filtros,
        ls_filtros       TYPE zftpme_filtros,
        ls_t001k         TYPE  t001k.

***  Checa se perido está aberto/fechado
  CALL FUNCTION 'AIP01_PLANT_DETERMINE'
    EXPORTING
      i_werks  = wa_equi-swerk
    IMPORTING
      es_t001k = ls_t001k
    EXCEPTIONS
      OTHERS   = 1.

  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = v_dt_ponto
      p_bukrs        = ls_t001k-bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    CONCATENATE 'A data informada esta em um período bloqueado para a empresa ' ls_t001k-bukrs '.' INTO ls_return-message SEPARATED BY space.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    WA_RETMED-TIPO = 'ECONT'.
*    APPEND WA_RETMED TO IT_RETMED.
*    CLEAR WA_RETMED.

    p_erro = 'X'.
  ENDIF.

  " Teste Combustivel
  IF NOT wa_comb-comb IS INITIAL OR NOT wa_comb-quant IS INITIAL.

    IF v_medato = v_medano AND v_nerro IS NOT INITIAL.
      ls_return-message = 'Para apontamentos de abastecimento é necessario atualização de posição do contador.'.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
      p_erro = 'X'.
      EXIT.
    ENDIF.

*    PERFORM ZF_CHECK_COMB CHANGING P_ERRO.
  ENDIF.

  " Teste Filtros
  CLEAR it_sistemas[].
  lt_filtros[] = it_saida2[].
  DELETE lt_filtros WHERE code     IS INITIAL
                     AND  code_ref IS INITIAL
                     AND  posto    IS INITIAL
                     AND  readg    IS INITIAL
                     AND  motivo   IS INITIAL.

  LOOP AT lt_filtros INTO ls_filtros.

    CLEAR: lt_return[], lt_item, ls_header, ls_code, v_tabix.

    v_tabix = sy-tabix.

    PERFORM zf_check_tbfiltro USING ls_filtros
                              CHANGING p_erro.

    READ TABLE it_sistemas WITH KEY sistema = ls_filtros-sistema.
    IF sy-subrc = 0.

      CONCATENATE 'Não é possível realizar apontamentos de remonta e troca para um mesmo compartimento (' ls_filtros-sistema ').' INTO ls_return-message.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.

*      CONCATENATE 'Não é possível realizar apontamentos de remonta e troca para um mesmo compartimento (' LS_FILTROS-SISTEMA ').' INTO WA_RETMED-MESSAGE.
*      WA_RETMED-TIPO = 'EOLEO'.
*      APPEND WA_RETMED TO IT_RETMED.
*      CLEAR WA_RETMED.

      p_erro = 'X'.
    ENDIF.
    it_sistemas-sistema = ls_filtros-sistema.
    APPEND it_sistemas.
  ENDLOOP.

  " Teste Lubrificantes
  CLEAR it_sistemas[].
  lt_lubrificantes[] = it_saida1[].
  DELETE lt_lubrificantes WHERE code     IS INITIAL
                           AND  code_ref IS INITIAL
                           AND  posto    IS INITIAL
                           AND  motivo   IS INITIAL
                           AND  readg    IS INITIAL.

  LOOP AT lt_lubrificantes INTO ls_lubrificantes.

    CLEAR: lt_return[], lt_item, ls_header, ls_code, v_tabix.

    v_tabix = sy-tabix.

    PERFORM zf_check_tboleo USING ls_lubrificantes
                            CHANGING p_erro.

    READ TABLE it_sistemas WITH KEY sistema = ls_lubrificantes-sistema.
    IF sy-subrc = 0.

      CONCATENATE 'Não é possível realizar apontamentos de remonta e troca para um mesmo compartimento (' ls_lubrificantes-sistema ').' INTO ls_return-message.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.

*      CONCATENATE 'Não é possível realizar apontamentos de remonta e troca para um mesmo compartimento (' LS_LUBRIFICANTES-SISTEMA ').' INTO WA_RETMED-MESSAGE.
*      WA_RETMED-TIPO = 'EOLEO'.
*      APPEND WA_RETMED TO IT_RETMED.
*      CLEAR WA_RETMED.

      p_erro = 'X'.
    ENDIF.
    it_sistemas-sistema = ls_lubrificantes-sistema.
    APPEND it_sistemas.
  ENDLOOP.

  IF p_erro = 'X'.
    wg_erro = p_erro.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_MEDATO  text
*----------------------------------------------------------------------*
FORM zf_save_cont  USING    p_value.

  DATA: it_not          TYPE STANDARD TABLE OF bapi2080_1,
        ls_not          TYPE bapi2080_1,
        lv_resp         TYPE c LENGTH 1,
        lv_docu         TYPE imrg-mdocm,
        lv_nota         TYPE qmel-qmnum,
        lv_msg          TYPE c LENGTH 100,
        ls_doc_complete TYPE imrg,
*        LS_RETURN   TYPE BAPIRET2,
*        LT_RETURN   TYPE STANDARD TABLE OF BAPIRET2,
        lv_value        TYPE rimr0-recdc,
        lv_val          TYPE ztparam-zval,
        lv_tpnota       TYPE qmel-qmart,
        lv_errsubrc     TYPE c LENGTH 50,
        lv_point        TYPE diimpt-point.

  CLEAR: wa_retmed, lv_value, lv_errsubrc.

  lv_value = p_value.
  REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
  CONDENSE lv_value NO-GAPS.

  MOVE v_pointo TO lv_point.

  CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
    EXPORTING
      measurement_point    = lv_point
      secondary_index      = ' '
      reading_date         = v_dt_ponto
      reading_time         = v_hr_ponto
      short_text           = ' '
      reader               = sy-uname
      origin_indicator     = ' ' "'A'
      reading_after_action = ' '
      recorded_value       = lv_value
      recorded_unit        = ' '
      difference_reading   = ' '
      code_version         = ' '
      check_custom_duprec  = ' '
      with_dialog_screen   = ' '
      prepare_update       = abap_true
      commit_work          = abap_true
      wait_after_commit    = abap_true
      create_notification  = ' '
      notification_type    = ' '
      notification_prio    = ' '
    IMPORTING
      measurement_document = lv_docu
      complete_document    = ls_doc_complete
      notification         = lv_nota
    EXCEPTIONS
      no_authority         = 1
      point_not_found      = 2
      index_not_unique     = 3
      type_not_found       = 4
      point_locked         = 5
      point_inactive       = 6
      timestamp_in_future  = 7
      timestamp_duprec     = 8
      unit_unfit           = 9
      value_not_fltp       = 10
      value_overflow       = 11
      value_unfit          = 12
      value_missing        = 13
      code_not_found       = 14
      notif_type_not_found = 15
      notif_prio_not_found = 16
      notif_gener_problem  = 17
      update_failed        = 18
      invalid_time         = 19
      invalid_date         = 20
      OTHERS               = 21.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        lv_errsubrc = TEXT-065.
      WHEN 2.
        lv_errsubrc = TEXT-066.
      WHEN 3.
        lv_errsubrc = TEXT-067.
      WHEN 4.
        lv_errsubrc = TEXT-068.
      WHEN 5.
        lv_errsubrc = TEXT-069.
      WHEN 6.
        lv_errsubrc = TEXT-070.
      WHEN 7.
        lv_errsubrc = TEXT-071.
      WHEN 8.
        lv_errsubrc = TEXT-072.
      WHEN 9.
        lv_errsubrc = TEXT-073.
      WHEN 10.
        lv_errsubrc = TEXT-074.
      WHEN 11.
        lv_errsubrc = TEXT-074.
      WHEN 12.
        lv_errsubrc = TEXT-074.
      WHEN 13.
        lv_errsubrc = TEXT-075.
      WHEN 14.
        lv_errsubrc = TEXT-076.
      WHEN 19.
        lv_errsubrc = TEXT-077.
      WHEN 20.
        lv_errsubrc = TEXT-072.
      WHEN OTHERS.
        lv_errsubrc = TEXT-098.
    ENDCASE.

*    WA_RETMED-TIPO = 'ECONT'.
    CONCATENATE TEXT-044 wa_dimpt-pttxt TEXT-054 TEXT-064 lv_errsubrc INTO ls_return-message SEPARATED BY space.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
*    APPEND WA_RETMED TO IT_RETMED
*    CLEAR WA_RETMED.

*    WA_RETMED1-TIPO = 'E'.
*    APPEND WA_RETMED1 TO IT_RETMED1.
*    CLEAR WA_RETMED1.

    p_erro = 'X'.
    EXIT.

  ELSE.
    CONCATENATE TEXT-041 lv_docu TEXT-055 wa_dimpt-pttxt TEXT-042
           INTO ls_return-message SEPARATED BY space.

    PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                CHANGING et_return.

    APPEND VALUE #( mdocm =  lv_docu ) TO t_doc_med.
*    APPEND WA_RETMED TO IT_RETMED.
*    CLEAR WA_RETMED.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE_LUBRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_save_lubri .

  DATA: lt_lubrificantes   TYPE STANDARD TABLE OF ty_zftpme_lubrificante,
        ls_lubrificantes   TYPE ty_zftpme_lubrificante,
        lv_point           TYPE imrg-point,
        lv_measurement_doc TYPE imrg-mdocm,
        ls_doc_complete    TYPE imrg,
        ls_notification    TYPE qmel-qmnum,
        lv_valor           TYPE ztparam-zval,
        lv_value           TYPE rimr0-recdc,
        lv_errsubrc        TYPE c LENGTH 50,
        lv_motivo          TYPE imrg-mdtxt,
        wa_zpmr0009        TYPE zpmr0009.

  DATA: ls_header  TYPE bapi2017_gm_head_01,
        ls_code    TYPE bapi2017_gm_code,
        ls_item    TYPE bapi2017_gm_item_create,
        lt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_return  TYPE STANDARD TABLE OF bapiret2,
        lw_return  TYPE  bapiret2,
        ls_testrun TYPE STANDARD TABLE OF bapi2017_gm_gen-testrun,
        lw_testrun TYPE bapi2017_gm_gen,
        lv_mater   TYPE bapi2017_gm_head_ret-mat_doc,
        lv_matkl   TYPE mara-matkl.

  CLEAR: it_ret[], wa_retmed, lt_return[], lv_value, lv_errsubrc.

  lt_lubrificantes[] = it_saida1[].

  LOOP AT lt_lubrificantes INTO ls_lubrificantes.
    READ TABLE it_dimpt INTO wa_dimpt WITH KEY locas = ls_lubrificantes-sistema mptyp = 'H'.
    IF sy-subrc = 0.
      lv_point = wa_dimpt-point.

      CLEAR lv_value.
      lv_value = ls_lubrificantes-readg.

      REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
      CONDENSE lv_value NO-GAPS.

      lv_motivo = |{ ls_lubrificantes-motivo } - { ls_lubrificantes-desc_motivo }|.

***      Valida materias
      ls_return-message = |{ TEXT-041 } { lv_measurement_doc } { TEXT-042 } |.
      PERFORM zf_inserir_mensagem USING c_s 999 space space ls_return-message  space CHANGING et_return.

      " Grava Apontamento de Consumo
      CLEAR: ls_header, lt_item[], ls_item.

      ls_header-pstng_date = v_dt_ponto.
      ls_header-doc_date   = v_dt_ponto.

      UNPACK ls_lubrificantes-ordem TO ls_header-ref_doc_no.
      UNPACK ls_lubrificantes-ordem TO ls_item-orderid.

      ls_code-gm_code      = '03'.

      "*---> 28/06/2023 - Migração S4 - LO
*      ls_item-material = |{ ls_lubrificantes-code_ref ALPHA = IN }|.

      DATA(v_len) = strlen( ls_lubrificantes-code_ref ).

      IF v_len > 18.
        ls_item-material_long = |{ ls_lubrificantes-code_ref ALPHA = IN }|.
      ELSE.
        ls_item-material      = |{ ls_lubrificantes-code_ref ALPHA = IN }|.
      ENDIF.
      "*<--- 28/06/2023 - Migração S4 - LO
      ls_item-plant        = wa_equi-swerk.
      ls_item-move_type    = '261'.
      ls_item-entry_qnt    = ls_lubrificantes-readg.

      SELECT SINGLE storage
      FROM t370fld_stn
      INTO ls_item-stge_loc
      WHERE station = ls_lubrificantes-posto.

      wa_input-ib_ordem = |{ wa_input-ib_ordem ALPHA = IN }|.

      SELECT SINGLE vornr
        FROM afvc AS a
       INNER JOIN afko AS b ON a~aufpl = b~aufpl
        INTO ls_item-activity
       WHERE b~aufnr = wa_input-ib_ordem.

      APPEND ls_item TO lt_item.
      CLEAR ls_item.

      "Grava número da Amostra na tabela IMRG_USR
      IF ls_lubrificantes-code EQ '0040' AND ls_lubrificantes-namostra IS NOT INITIAL.
        MOVE ls_lubrificantes-namostra TO imrg_usr-amostra.
      ENDIF.

      IF v_nbaixa IS INITIAL AND ls_lubrificantes-code NE '0040'.             "/Modificação CS2016001593

        lw_testrun-testrun = ' '.
        APPEND lw_testrun TO ls_testrun.

        "*---> 28/06/2023 - Migração S4 - LO
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            goodsmvt_header  = ls_header
            goodsmvt_code    = ls_code
          IMPORTING
            materialdocument = lv_mater
          TABLES
            goodsmvt_item    = lt_item
            return           = lt_return.

        CLEAR: ls_header, ls_code, lt_item[].

        IF sy-subrc = 0 AND
           lt_return IS INITIAL.
          CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
            EXPORTING
              measurement_point    = lv_point
              reading_date         = v_dt_ponto
              reading_time         = v_hr_ponto
              reader               = lv_reader
              origin_indicator     = 'A'
              recorded_value       = lv_value
              difference_reading   = abap_true
              valuation_code       = ls_lubrificantes-code
              short_text           = lv_motivo
              prepare_update       = 'X'
              commit_work          = 'X'
              wait_after_commit    = 'X'
              user_data            = imrg_usr
            IMPORTING
              measurement_document = lv_measurement_doc
              complete_document    = ls_doc_complete
              notification         = ls_notification
            EXCEPTIONS
              no_authority         = 1
              point_not_found      = 2
              index_not_unique     = 3
              type_not_found       = 4
              point_locked         = 5
              point_inactive       = 6
              timestamp_in_future  = 7
              timestamp_duprec     = 8
              unit_unfit           = 9
              value_not_fltp       = 10
              value_overflow       = 11
              value_unfit          = 12
              value_missing        = 13
              code_not_found       = 14
              notif_type_not_found = 15
              notif_prio_not_found = 16
              notif_gener_problem  = 17
              update_failed        = 18
              invalid_time         = 19
              invalid_date         = 20
              OTHERS               = 21.
          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1. lv_errsubrc = TEXT-065.
              WHEN 2. lv_errsubrc = TEXT-066.
              WHEN 3. lv_errsubrc = TEXT-067.
              WHEN 4. lv_errsubrc = TEXT-068.
              WHEN 5. lv_errsubrc = TEXT-069.
              WHEN 6. lv_errsubrc = TEXT-070.
              WHEN 7. lv_errsubrc = TEXT-071.
              WHEN 8. lv_errsubrc = TEXT-072.
              WHEN 9. lv_errsubrc = TEXT-073.
              WHEN 10.lv_errsubrc = TEXT-074.
              WHEN 11.lv_errsubrc = TEXT-074.
              WHEN 12.lv_errsubrc = TEXT-074.
              WHEN 13.lv_errsubrc = TEXT-075.
              WHEN 14.lv_errsubrc = TEXT-076.
              WHEN 19.lv_errsubrc = TEXT-077.
              WHEN 20.lv_errsubrc = TEXT-072.
              WHEN OTHERS.
                lv_errsubrc = TEXT-098.
            ENDCASE.

            ls_return-message = |{ TEXT-046 } { TEXT-064 } { lv_errsubrc } |.
            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
            p_erro = abap_true.
            EXIT.
          ELSE.
            IF lv_mater IS NOT INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              wa_input-doc_material = lv_mater.

              CONCATENATE TEXT-053 lv_mater TEXT-042 INTO ls_return-message SEPARATED BY space.

              PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                          CHANGING et_return.

              APPEND VALUE #( mdocm =  lv_measurement_doc ) TO t_doc_med.

            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              LOOP AT  lt_return INTO lw_return.
                PERFORM zf_ins_mensagem USING c_e lw_return-number lw_return-id space space lw_return-message space CHANGING et_return.
                p_erro = 'X'.
                EXIT.
              ENDLOOP.
            ENDIF.
          ENDIF.

          IF ls_lubrificantes-plano IS NOT INITIAL.
            IF ls_lubrificantes-code NE '0040'.                               "/Modificação CS2016001593
              PERFORM zf_ence_order USING ls_lubrificantes-ordem.
            ENDIF.
          ENDIF.

          IF ls_lubrificantes-code = '0010'.
            IF ls_lubrificantes-plano IS INITIAL.
              PERFORM zf_busca_plano USING ls_lubrificantes-sistema CHANGING ls_lubrificantes-plano.
            ENDIF.

            IF ls_lubrificantes-plano IS INITIAL.
              ls_return-message = 'Não foi possivel reinicar um plano para a ordem.'.
              PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
              p_erro = abap_true.
              EXIT.
            ELSE.
              PERFORM zf_shdb USING 'IP10' ls_lubrificantes-ordem ls_lubrificantes-sistema ls_lubrificantes-plano.
***           Gera Log
              CLEAR wa_log.
              wa_log-mensagem = |Reiniciado plano: { ls_lubrificantes-plano }|.
              APPEND wa_log TO it_log.
            ENDIF.
          ENDIF.

          ls_return-message = |{ TEXT-053 } { lv_mater } { TEXT-042 }|.
          PERFORM zf_inserir_mensagem USING c_s 999 space space ls_return-message  space CHANGING et_return.
        ENDIF.
      ELSE.
        LOOP AT lt_return INTO ls_return.
          PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
        ENDLOOP.
      ENDIF.
      CLEAR: imrg_usr.
    ELSE.
      ls_return-message = |{ TEXT-026 } { lv_valor }|.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
      p_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAVE_FILTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_save_filtro .

  DATA: lv_point           TYPE imrg-point,
        lv_value           TYPE rimr0-recdc,
        lv_index           TYPE sy-tabix,
        lv_measurement_doc TYPE imrg-mdocm,
        lv_valor           TYPE ztparam-zval,
        lv_message         TYPE c LENGTH 100,
        lv_tabix           TYPE sy-tabix,
        lv_errsubrc        TYPE c LENGTH 50,
        lv_ordem           TYPE caufvd-warpl,
        lv_motivo          TYPE imrg-mdtxt.

  DATA: ls_filtros      TYPE ty_zftpme_filtros,
        lt_filtros      TYPE STANDARD TABLE OF ty_zftpme_filtros,
        ls_doc_complete TYPE imrg,
        ls_notification TYPE qmel-qmnum.

  DATA: ls_header  TYPE bapi2017_gm_head_01,
        ls_code    TYPE bapi2017_gm_code,
        ls_item    TYPE bapi2017_gm_item_create,
        lt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_return  TYPE STANDARD TABLE OF bapiret2,
        lw_return  TYPE bapiret2,
        ls_testrun TYPE STANDARD TABLE OF bapi2017_gm_gen-testrun,
        lw_testrun TYPE bapi2017_gm_gen,
*        LS_RETURN TYPE BAPIRET2,
        lv_mater   TYPE bapi2017_gm_head_ret-mat_doc,
        lv_matkl   TYPE mara-matkl.

*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      PERCENTAGE = SY-INDEX
*      TEXT       = 'Salvando dados de troca de filtros...'.

  CLEAR: it_return[], wa_retmed, lt_return, lv_errsubrc.

  DELETE it_saida2 WHERE code IS INITIAL
                     AND code_ref IS INITIAL
                     AND motivo IS INITIAL
                     AND readg IS INITIAL.

  IF it_saida2[] IS INITIAL.
    ls_return-message = TEXT-024.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ELSE.
    CLEAR: it_ret[].
    lt_filtros[] = it_saida2[].

    LOOP AT lt_filtros INTO ls_filtros.
      lv_tabix = sy-tabix.
      READ TABLE it_dimpt INTO wa_dimpt WITH KEY locas = ls_filtros-sistema
                                                 mptyp = 'F'.
      IF sy-subrc = 0.
        CLEAR lv_point.
        lv_point = wa_dimpt-point.

        CLEAR lv_value.
        lv_value = ls_filtros-readg.

        REPLACE ALL OCCURRENCES OF '.' IN lv_value WITH ','.
        CONDENSE lv_value NO-GAPS.

        IF sy-subrc = 0
          AND lt_return IS INITIAL.

          CONCATENATE ls_filtros-motivo '-' ls_filtros-desc_motivo INTO lv_motivo SEPARATED BY space.

          CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
            EXPORTING
              measurement_point    = lv_point
              reading_date         = v_dt_ponto
              reading_time         = v_hr_ponto
              reader               = lv_reader
              origin_indicator     = 'A'
              recorded_value       = lv_value
              difference_reading   = abap_true
              short_text           = lv_motivo
              valuation_code       = ls_filtros-code
              prepare_update       = 'X'
              commit_work          = 'X'
              wait_after_commit    = 'X'
            IMPORTING
              measurement_document = lv_measurement_doc
              complete_document    = ls_doc_complete
              notification         = ls_notification
            EXCEPTIONS
              no_authority         = 1
              point_not_found      = 2
              index_not_unique     = 3
              type_not_found       = 4
              point_locked         = 5
              point_inactive       = 6
              timestamp_in_future  = 7
              timestamp_duprec     = 8
              unit_unfit           = 9
              value_not_fltp       = 10
              value_overflow       = 11
              value_unfit          = 12
              value_missing        = 13
              code_not_found       = 14
              notif_type_not_found = 15
              notif_prio_not_found = 16
              notif_gener_problem  = 17
              update_failed        = 18
              invalid_time         = 19
              invalid_date         = 20
              OTHERS               = 21.

          IF sy-subrc <> 0.
            CASE sy-subrc.
              WHEN 1.lv_errsubrc = TEXT-065.
              WHEN 2.lv_errsubrc = TEXT-066.
              WHEN 3.lv_errsubrc = TEXT-067.
              WHEN 4.lv_errsubrc = TEXT-068.
              WHEN 5.lv_errsubrc = TEXT-069.
              WHEN 6.lv_errsubrc = TEXT-070.
              WHEN 7.lv_errsubrc = TEXT-071.
              WHEN 8.lv_errsubrc = TEXT-072.
              WHEN 9.lv_errsubrc = TEXT-073.
              WHEN 10.lv_errsubrc = TEXT-074.
              WHEN 11.lv_errsubrc = TEXT-074.
              WHEN 12.lv_errsubrc = TEXT-074.
              WHEN 13.lv_errsubrc = TEXT-075.
              WHEN 14.lv_errsubrc = TEXT-076.
              WHEN 19.lv_errsubrc = TEXT-077.
              WHEN 20.lv_errsubrc = TEXT-072.
              WHEN OTHERS.
                lv_errsubrc = TEXT-098.
            ENDCASE.

            ls_return-message = |{ TEXT-045 } { TEXT-064 } { lv_errsubrc  }|.
            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
            p_erro = abap_true.
            EXIT.
          ELSE.
            ls_return-message = |{ TEXT-041 } { lv_measurement_doc } { TEXT-042 }|.
            PERFORM zf_inserir_mensagem USING c_s 999 space space ls_return-message  space CHANGING et_return.

            CLEAR: ls_header, lt_item[], ls_item.

            " Grava Apontamento de Consumo
            ls_header-pstng_date = v_dt_ponto.
            ls_header-doc_date   = v_dt_ponto.

            UNPACK ls_filtros-ordem TO ls_header-ref_doc_no.
            UNPACK ls_filtros-ordem TO ls_item-orderid.

            ls_code-gm_code      = '03'.
            "*---> 28/06/2023 - Migração S4 - LO
*            ls_item-material     = |{ ls_filtros-code_ref ALPHA = IN  }|.

            DATA(v_mat) = |{ ls_filtros-code_ref ALPHA = IN  }|.
            DATA(v_len) = strlen( v_mat ).

            IF v_len > 18.
              ls_item-material_long = v_mat.
            ELSE.
              ls_item-material      = v_mat.
            ENDIF.
            CLEAR v_mat.
            "*<--- 28/06/2023 - Migração S4 - LO

            ls_item-plant        = wa_equi-swerk.
            ls_item-move_type    = '261'.
            ls_item-entry_qnt    = ls_filtros-readg.

            SELECT SINGLE storage
            FROM t370fld_stn
            INTO ls_item-stge_loc
            WHERE station = ls_filtros-posto.

            wa_input-ib_ordem = |{ wa_input-ib_ordem ALPHA = IN }|.

            SELECT SINGLE vornr
              FROM afvc AS a
             INNER JOIN afko AS b ON a~aufpl = b~aufpl
              INTO ls_item-activity
             WHERE b~aufnr = wa_input-ib_ordem.

            APPEND ls_item TO lt_item.
            CLEAR ls_item.

            IF ls_filtros-plano IS NOT INITIAL.
              PERFORM zf_ence_order USING ls_filtros-ordem.
            ENDIF.

            IF ls_filtros-code = '0010'.
              IF ls_filtros-plano IS INITIAL.
                PERFORM zf_busca_plano USING ls_filtros-sistema CHANGING ls_filtros-plano.
              ENDIF.
              PERFORM zf_shdb USING 'IP10' ls_filtros-ordem ls_filtros-sistema ls_filtros-plano.
***           Gera Log
              CLEAR wa_log.
              CONCATENATE 'Reiniciado plano:' ls_filtros-plano INTO wa_log-mensagem SEPARATED BY space.
              APPEND wa_log TO it_log.

              lw_testrun-testrun = ' '.
              APPEND lw_testrun TO ls_testrun.

              IF v_nbaixa IS INITIAL.
                "*---> 28/06/2023 - Migração S4 - LO
                CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
                  EXPORTING
                    goodsmvt_header  = ls_header
                    goodsmvt_code    = ls_code
                  IMPORTING
                    materialdocument = lv_mater
                  TABLES
                    goodsmvt_item    = lt_item
                    return           = lt_return.

                CLEAR: ls_header, ls_code, lt_item[].
              ENDIF.
            ENDIF.

            IF lv_mater IS NOT INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              wa_input-doc_material = lv_mater.

              CONCATENATE TEXT-053 lv_mater TEXT-042 INTO ls_return-message SEPARATED BY space.

              PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                          CHANGING et_return.



            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              LOOP AT  lt_return INTO lw_return.
                PERFORM zf_ins_mensagem USING c_e lw_return-number lw_return-id space space lw_return-message space CHANGING et_return.
                p_erro = 'X'.
                EXIT.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSE.
          ls_return-message = |{ TEXT-026 } { lv_valor }|.
          PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
          p_erro = abap_true.
          EXIT.
        ENDIF.

      ELSE.
        lv_message = |{ TEXT-026 } { lv_valor } |.
        ls_return-message = lv_message.
        PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
        p_erro = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_TEMP_TIPO  text
*      -->P_TL_TEMP_ORDEM  text
*      -->P_4464   text
*      -->P_4465   text
*----------------------------------------------------------------------*
FORM zf_shdb USING p_tipo
                   p_ordem   TYPE zftpme_lubrificante-ordem
                   p_sistema TYPE zftpme_lubrificante-sistema
                   p_plano    TYPE rmipm-warpl.

  DATA: vl_data   TYPE c LENGTH 10,
        vl_dt_sys TYPE c LENGTH 10,
        vl_hora   TYPE c LENGTH 5,
        vl_tplnr  TYPE c LENGTH 10,
        vl_meda_t TYPE c LENGTH 30,
        vl_medato TYPE rmipm-szaeh.

  REFRESH it_bdcdata.

  CONCATENATE  v_dt_ponto+6(2) v_dt_ponto+4(2) v_dt_ponto(4) INTO vl_data SEPARATED BY '.'.
  CONCATENATE  sy-datum+6(2) sy-datum+4(2) v_dt_ponto(4) INTO vl_dt_sys SEPARATED BY '.'.
  CONCATENATE wa_equi-swerk '-OFI-PM-01' INTO vl_tplnr.

  vl_meda_t = v_medato.

  CONDENSE vl_meda_t NO-GAPS.
  MOVE vl_meda_t TO vl_medato.

  vl_hora = v_hr_ponto(5).

  CASE p_tipo.
    WHEN 'IW32'.
*      PERFORM ZF_BDC_DATA USING:
*        ''          ''      'T'  'IW32'             'BS AA X   F',
*        'SAPLCOIH'  '0101'  'X'  ''                 ' ',
*        ''          ''      ' '  'BDC_CURSOR'       'CAUFVD-AUFNR',
*        ''          ''      ' '  'BDC_OKCODE'       '/00',
*        ''          ''      ' '  'CAUFVD-AUFNR'     P_ORDEM,
*        'SAPLCOIH'  '3000'  'X'  ' '                ' ',
*        ''          ''      ' '  'BDC_OKCODE'       '=ARCH',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                3001SUB_ALL',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                1100SUB_LEVEL',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                1102SUB_KOPF',
*        ''          ''      ' '  'BDC_CURSOR'       'CAUFVD-KTEXT',
*        ''          ''      ' '  'CAUFVD-KTEXT'     ' ',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                1105SUB_BTN',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                1104SUB_TEXT',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                1120SUB_AUFTRAG',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLIPAR                                0415SUB_ADRESSE',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLIPAR                                0415SUB_ADDR_PM',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                0154HEADER',
*        ''          ''      ' '  'CAUFVD-INGPR'     'VEI',
*        ''          ''      ' '  'CAUFVD-VAPLZ'     'OFICINA',
*        ''          ''      ' '  'CAUFVD-VAWRK'     WA_EQUI-SWERK,
*        ''          ''      ' '  'CAUFVD-ILART'     'Z03',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                0153MAINORDER',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLIPAR                                0415PARTNER',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                7401SUB_PM_ADDR',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                7300TERM',
*        ''          ''      ' '  'CAUFVD-GSTRP'     VL_DATA,
*        ''          ''      ' '  'CAUFVD-PRIOK'     '4',
*        ''          ''      ' '  'CAUFVD-GLTRP'     VL_DT_SYS,
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                7301SUB_BTN',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                7310SUB_ADD',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                7100OBJECT',
**        ''          ''      ' '  'CAUFVD-TPLNR'     vl_tplnr,
*        ''          ''      ' '  'CAUFVD-EQUNR'     V_EQUIP,
*        ''          ''      ' '  'CAUFVD-BAUTL'     P_SISTEMA,
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                0815NOTIFICATION_DATA',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOI0                                0310AVO',
*        ''          ''      ' '  'AFVGD-LTXA1'      ' ',"Mudar pa descrição do sistema
*        ''          ''      ' '  'AFVGD-ARBPL'      'OFICINA',
*        ''          ''      ' '  'AFVGD-WERKS'      WA_EQUI-SWERK,
**        ''          ''      ' '  'AFVGD-STEUS'      'PM01',
*        ''          ''      ' '  'AFVGD-ARBEI'      '2,0',
*        ''          ''      ' '  'AFVGD-ARBEH'      'H',
*        ''          ''      ' '  'BDC_SUBSCR'       'SAPLCOIH                                0153SUB_SERVICE',
*        'SAPLCOI0'  '1000'  'X'  ' '                ' ',
*        ''          ''      ' '  'BDC_CURSOR'       'RIARCH-ADDAT',
*        ''          ''      ' '  'BDC_OKCODE'       '=WEIT',
*        ''          ''      ' '  'RIARCH-ADDAT'     VL_DATA,
*        ''          ''      ' '  'RIARCH-ADUHR'     VL_HORA,
*        ''          ''      ' '  'RIARCH-CLOSE_NOTIFIC'    'X',
*        ''          ''      ' '  'BDC_SUBSCR'       'APLIQS0                                6030NOTIFICATION'.
*      PERFORM ZF_CALL_TRANSACTION USING 'IW32' 'N' 'S'.
    WHEN 'IP10'.
      PERFORM zf_bdc_data USING:
        ''          ''      'T' 'IP10'            'BS AA XXX F',
        'SAPLIWP3'  '0140'  'X' ''                '',
        ''          ''      ''  'BDC_CURSOR'      'RMIPM-WARPL',
        ''          ''      ''  'BDC_OKCODE'      '/00',
        ''          ''      ''  'RMIPM-WARPL'	    p_plano,
        'SAPLIWP3'  '103'   'X' ''                '',
        ''          ''      ''  'BDC_OKCODE'      '=NS',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                6000WPLANKOPF',
        ''          ''      ''  'BDC_CURSOR'      'RMIPM-WARPL',
        'SAPLSPO2'  '0101'  'X' ''                '',
        ''          ''      ''  'BDC_OKCODE'      '=OPT2',
        'SAPLIWP3'  '7001'  'X' ''                '',
        ''          ''      ''  'BDC_CURSOR'      'RMIPM-SZAEH',
        ''          ''      ''  'BDC_OKCODE'      '=ENTR',
        ''          ''      ''  'RMIPM-SZAEH'	    vl_medato,
        'SAPLIWP3'  '0103'  'X' ''                '',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                8003SUBSCREEN_SCHEDULING',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                8031SUBSCREEN_BODY',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                0121SUBSCREEN_CALL',
        'SAPLIWP3'  '103'   'X' ''                '',
        ''          ''      ''  'BDC_OKCODE'      '=BU',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                6000WPLANKOPF',
        ''          ''      ''  'BDC_CURSOR'      'RMIPM-WARPL',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                8003SUBSCREEN_SCHEDULING',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                8031SUBSCREEN_BODY',
        ''          ''      ''  'BDC_SUBSCR'      'SAPLIWP3                                0121SUBSCREEN_CALL'.

      PERFORM zf_call_transaction USING 'IP10' 'N' 'S'.
    WHEN 'IW41'.
      PERFORM zf_bdc_data USING:
        ''          ''      'T' 'IW41'          'BS AA X   F',
        'SAPLCORU'  '3000'  'X' ''              '',
        ''          ''      ''  'BDC_CURSOR'    'CORUF-RUECK',
        ''          ''      ''  'BDC_OKCODE'    '=ENTR',
        ''          ''      ''  'CORUF-AUFNR'	  p_ordem,""
        'SAPLCORU'  '3200'  'X' ''              '',
        ''          ''      ''  'BDC_CURSOR'    'AFRUD-ISMNW_2',
*        ''          ''      ''  'BDC_OKCODE'    '=BU',
        ''          ''      ''  'AFRUD-ARBPL'	  'OFICINA',
        ''          ''      ''  'AFRUD-WERKS'	  wa_equi-swerk,
        ''          ''      ''  'AFRUD-PERNR'	  '2',
        ''          ''      ''  'AFRUD-ISMNW_2'	'2',
        ''          ''      ''  'AFRUD-ISMNU'	  'H',
        ''          ''      ''  'AFRUD-BUDAT'	  vl_data,
        ''          ''      ''  'AFRUD-OFMNU'	  'H',
        ''          ''      ''  'AFRUD-ISDD'    vl_data,
        ''          ''      ''  'AFRUD-ISDZ'    vl_hora,
        ''          ''      ''  'AFRUD-IDAUE'	  'H',
        ''          ''      ''  'AFRUD-IEDD'    vl_data,
        ''          ''      ''  'AFRUD-IEDZ'    vl_hora.

      PERFORM zf_call_transaction USING 'IW41' 'N' 'S'.
    WHEN 'MBST'.
      PERFORM zf_bdc_data USING:
        ''          ''      'T' 'MBST'          '',
        'RMMDBTCH'  '1000'  'X' ''              ''.
      PERFORM zf_call_transaction USING 'MBST' 'N' 'S'.
  ENDCASE.

  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_COMB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_check_comb CHANGING p_erro.

  DATA: vl_saldo  TYPE bapiret2-message,

        vl_matnr  TYPE mara-matnr,
        tl_dimpt  TYPE STANDARD TABLE OF diimpt WITH HEADER LINE,
        lv_cont   TYPE i,

        it_imrg   TYPE STANDARD TABLE OF imrg,                                "/Modificação CS2016001593
        wa_imrg   TYPE imrg,                                                  "/Modificação CS2016001593
        text_comb TYPE imrg-mdtxt,                                            "/Modificação CS2016001593
        it_iflo   TYPE STANDARD TABLE OF iflo.

  CLEAR: wa_ret, tl_dimpt[], vl_saldo.

  tl_dimpt[] = it_dimpt[].

  DELETE tl_dimpt WHERE mptyp <> 'M'.
  DESCRIBE TABLE tl_dimpt LINES lv_cont.

  IF lv_cont > 1.
    ls_return-message = 'Pontos duplicados para o COMBUSTIVEL.'.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  READ TABLE tl_dimpt INTO wa_dimpt WITH KEY mptyp = 'M'.

  IF sy-subrc = 0.
    PERFORM zf_valida_mat
      USING
        'O'
      CHANGING
        wa_comb-comb.

    IF wa_comb-comb IS INITIAL.
      MOVE: TEXT-048 TO ls_return-message.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.
*      APPEND WA_RET TO IT_RET.
*      CLEAR WA_RET.
      p_erro = 'X'.
    ELSE.
      IF v_nbaixa IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_comb-comb
          IMPORTING
            output = vl_matnr.

        PERFORM zf_saldo_mat
          USING
            v_dt_ponto
            wa_equi-swerk
            vl_matnr
            wa_comb-posto
            wa_equi-daufn
            wa_comb-quant
          CHANGING
            p_erro.

*        IF VL_SALDO IS NOT INITIAL.
**          WA_RET-MESSAGE = VL_SALDO.
*          LS_RETURN-MESSAGE = VL_SALDO.
*
*          PERFORM ZF_INSERIR_MENSAGEM USING C_E 999 SPACE SPACE SPACE  SPACE
*                                      CHANGING ET_RETURN.
*
**          APPEND WA_RET TO IT_RET.
*          P_ERRO = 'X'.
*        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_comb-posto IS INITIAL.
      MOVE: TEXT-049 TO ls_return-message.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.

*      MOVE:
*            TEXT-049 TO WA_RET-MESSAGE.
*      APPEND WA_RET TO IT_RET.
*      CLEAR WA_RET.
      p_erro = 'X'.
    ENDIF.

    IF wa_comb-quant IS INITIAL.
      MOVE: TEXT-050 TO ls_return-message.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.

*      MOVE:
*            TEXT-050 TO WA_RET-MESSAGE.
*      APPEND WA_RET TO IT_RET.
*      CLEAR WA_RET.
      p_erro = 'X'.
    ENDIF.
    "Consistência do Local
    IF wa_comb-local IS INITIAL.
*      MOVE:
*            TEXT-094 TO WA_RET-MESSAGE.
*      APPEND WA_RET TO IT_RET.
*      CLEAR WA_RET.
*      P_ERRO = 'X'.
    ELSE.
      CLEAR it_iflo.

      SELECT *
        FROM iflo
        INTO TABLE it_iflo
        WHERE msgrp EQ wa_comb-local.

      READ TABLE it_iflo WITH KEY msgrp = wa_comb-local
                                  fltyp = 'C' TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.

        MOVE: TEXT-095 TO ls_return-message.
        PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                    CHANGING et_return.
        p_erro = abap_true.
      ENDIF.
    ENDIF.

    PERFORM zf_valida_ordem
      USING wa_equi-daufn 'A' '' ''
      CHANGING wa_equi-daufn
               wa_equi-warpl.

    IF wa_equi-daufn IS INITIAL.
      MOVE: TEXT-087 TO ls_return-message.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                  CHANGING et_return.

*      MOVE: TEXT-087 TO WA_RET-MESSAGE.
*      APPEND WA_RET TO IT_RET.
*      CLEAR WA_RET.
      p_erro = 'X'.
    ENDIF.
** Regra foi removida, a permissão de abastecimento será dada pelo parâmetro TP_OBJ na ZPM0002
*    IF WA_EQUI-EQTYP <> 'V'.
*      MOVE: 'Categoria de equipamento não permitida para abastecimento' TO WA_RET-MESSAGE.
*      APPEND WA_RET TO IT_RET.
*      CLEAR WA_RET.
*      P_ERRO = 'X'.
*    ENDIF.

** Checa duplicação de número de formulário para abastecimento                "/Modificação CS2016001593
    IF v_n_form IS NOT INITIAL.
      SELECT *
        FROM imrg
        INTO TABLE it_imrg
        WHERE point EQ v_pt_comb
          AND cancl NE abap_true.

      IF sy-subrc IS INITIAL.

        CONCATENATE 'Formulário N.' v_n_form INTO text_comb SEPARATED BY space.
        LOOP AT it_imrg INTO wa_imrg.
          IF wa_imrg-mdtxt EQ text_comb.
            MOVE: TEXT-093 TO ls_return-message.
            PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                        CHANGING et_return.

*            MOVE: TEXT-093 TO WA_RET-MESSAGE.
*            APPEND WA_RET TO IT_RET.
*            CLEAR WA_RET.
            p_erro = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ELSE.
    ls_return-message = 'Não há ponto de medição para o combustível, impossível apontar abastecimento.'.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.
ENDFORM.                    " ZF_CHECK_COMB
*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_TBFILTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FILTROS  text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_check_tbfiltro USING ls_lfiltros
                       TYPE zftpme_filtros
                       CHANGING p_erro.

  DATA: lv_tabix TYPE c LENGTH 4,
        vl_saldo TYPE bapiret2-message.


  CLEAR: wa_ret, lv_tabix, vl_saldo.

  lv_tabix = v_tabix.

  CONDENSE lv_tabix NO-GAPS.

***  Valida atividade
  PERFORM zf_valida_ativ
    USING
      'F'
    CHANGING
      ls_lfiltros-code.

  IF ls_lfiltros-code IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO ls_return-message SEPARATED BY space.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    MOVE:
*        TEXT-028 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

***  Valida Materiais
  PERFORM zf_valida_mat
    USING
      'F'
    CHANGING
      ls_lfiltros-code_ref.

  IF ls_lfiltros-code_ref IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.

    ls_return-message = TEXT-035.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.
*    MOVE:
*        TEXT-035 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ELSE.
    IF v_nbaixa IS INITIAL.
      PERFORM zf_saldo_mat
        USING
          v_dt_ponto
          wa_equi-swerk
          ls_lfiltros-code_ref
          ls_lfiltros-posto
          ls_lfiltros-ordem
          ls_lfiltros-readg
        CHANGING
          p_erro.

*      IF VL_SALDO IS NOT INITIAL.
*        CONCATENATE TEXT-027 LV_TABIX INTO WA_RET-TIPO SEPARATED BY SPACE.

*        LS_RETURN-MESSAGE = VL_SALDO.
*        PERFORM ZF_INSERIR_MENSAGEM USING C_E 999 SPACE SPACE SPACE  SPACE
*                                    CHANGING ET_RETURN.
*        APPEND WA_RET TO IT_RET.
*        P_ERRO = 'X'.
*      ENDIF.
    ENDIF.
  ENDIF.

  IF ls_lfiltros-readg IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE TEXT-031 TO WA_RET-MESSAGE.

    ls_return-message = TEXT-031.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  IF ls_lfiltros-motivo IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE TEXT-032 TO WA_RET-MESSAGE.

    ls_return-message = TEXT-032.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  IF ls_lfiltros-ordem IS INITIAL..
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.

    ls_return-message = TEXT-034.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    MOVE:
*        TEXT-034 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

ENDFORM.                    " ZF_CHECK_TBFILTROS
*&---------------------------------------------------------------------*
*&      Form  ZF_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7647   text
*      -->P_7648   text
*      -->P_7649   text
*      -->P_7650   text
*      -->P_7651   text
*----------------------------------------------------------------------*
FORM zf_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    "F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_ATIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8498   text
*      <--P_LS_LFILTROS_CODE  text
*----------------------------------------------------------------------*
FORM zf_valida_ativ USING p_tipo TYPE c
                    CHANGING c_ativ.
  IF c_ativ IS NOT INITIAL.
    CASE p_tipo.
      WHEN 'C'.
        CASE c_ativ.
          WHEN '0020' OR '0010' OR '0040'.                                    "/Modificação CS2016001593
            MESSAGE 'Atividade validada' TYPE 'S'.
          WHEN OTHERS.
            MESSAGE TEXT-022 TYPE 'I'.
            CLEAR c_ativ.
        ENDCASE.
      WHEN 'F'.
        CASE c_ativ.
          WHEN '0030' OR '0010'.
            MESSAGE 'Atividade validada' TYPE 'S'.
          WHEN OTHERS.
            MESSAGE TEXT-022 TYPE 'I'.
            CLEAR c_ativ.
        ENDCASE.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFORM.                    "zf_valida_ativ
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LUBRIFICANTES_SISTEMA  text
*      <--P_LS_LUBRIFICANTES_PLANO  text
*----------------------------------------------------------------------*
FORM zf_busca_plano  USING p_sistema TYPE mpos-bautl
  CHANGING p_pln_s TYPE mpos-warpl.

  DATA: lv_mpos  TYPE mpos-warpl,
        vl_equip TYPE mpos-equnr.

  CLEAR lv_mpos.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_equip
    IMPORTING
      output = vl_equip.

  SELECT SINGLE warpl
    FROM mpos
    INTO lv_mpos
    WHERE equnr = vl_equip AND
          bautl = p_sistema.

  IF sy-subrc = 0.
    p_pln_s = lv_mpos.
  ENDIF.
ENDFORM.                    " ZF_BUSCA_PLANO
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7922   text
*      -->P_7923   text
*      -->P_7924   text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans
                               p_mode
                               p_upd.
  DATA: BEGIN OF tl_msg OCCURS 0,
          msg TYPE t100-text,
        END OF tl_msg.

*  Data: LV_MSG TYPE BAPIRET2-MESSAGE.

  REFRESH it_msg.

  CALL TRANSACTION p_trans USING it_bdcdata
    MODE p_mode
    MESSAGES INTO it_msg
    UPDATE p_upd.

*  SELECT TEXT
*    FROM T100
*    INTO TABLE TL_MSG
*    FOR ALL ENTRIES IN IT_MSG
*    WHERE ARBGB = IT_MSG-MSGID AND
*          MSGNR = IT_MSG-MSGNR AND
*          SPRSL = SY-LANGU.
*
*  IF SY-SUBRC = 0.
*    LOOP AT TL_MSG.
*      CONCATENATE P_TRANS '-' TL_MSG-MSG INTO LV_MSG SEPARATED BY SPACE.
*      MESSAGE LV_MSG TYPE 'I'.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    "ZF_CALL_TRANSACTION.
*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_TBOLEO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LUBRIFICANTES  text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_check_tboleo USING ls_lubrificantes TYPE ty_zftpme_lubrificante "ZFTPME_LUBRIFICANTE
                    CHANGING p_erro.

  DATA: lv_tabix TYPE c LENGTH 4,
        vl_saldo TYPE bapiret2-message.


  CLEAR: wa_ret, lv_tabix, vl_saldo.

  lv_tabix = v_tabix.

  CONDENSE lv_tabix NO-GAPS.

***  Valida atividade
  PERFORM zf_valida_ativ
    USING
      'C'
    CHANGING
      ls_lubrificantes-code.

  IF ls_lubrificantes-code IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE:
    ls_return-message = TEXT-028.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  IF ls_lubrificantes-sistema IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE TEXT-088 TO WA_RET-MESSAGE.

    ls_return-message = TEXT-088.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

***  Valida Materiais
  PERFORM zf_valida_mat
    USING
      'C'
    CHANGING
      ls_lubrificantes-code_ref.

  IF ls_lubrificantes-code_ref IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE TEXT-029 TO WA_RET-MESSAGE.

    ls_return-message = TEXT-029.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                                CHANGING et_return.

*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ELSE.
    IF v_nbaixa IS INITIAL AND ls_lubrificantes-code NE '0040'.               "/Modificação CS2016001593
      PERFORM zf_saldo_mat
        USING
          v_dt_ponto
          wa_equi-swerk
          ls_lubrificantes-code_ref
          ls_lubrificantes-posto
          ls_lubrificantes-ordem
          ls_lubrificantes-readg
        CHANGING
          p_erro.

***      IF VL_SALDO IS NOT INITIAL.
***        CONCATENATE TEXT-027 LV_TABIX INTO WA_RET-TIPO SEPARATED BY SPACE.
****        WA_RET-MESSAGE = VL_SALDO.
***
***        LS_RETURN-MESSAGE = VL_SALDO.
***        PERFORM ZF_INSERIR_MENSAGEM USING C_E 999 SPACE SPACE SPACE  SPACE
***                                    CHANGING ET_RETURN.
***
****        APPEND WA_RET TO IT_RET.
***        P_ERRO = 'X'.
***      ENDIF.
    ENDIF.
  ENDIF.

***  Valida Posto
  IF ls_lubrificantes-posto IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE TEXT-030 TO WA_RET-MESSAGE.

    ls_return-message = TEXT-030.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.

*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  IF ls_lubrificantes-readg IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.
*    MOVE:
*        TEXT-031 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.

    ls_return-message = TEXT-031.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.

    p_erro = 'X'.
  ENDIF.

  IF ls_lubrificantes-motivo IS INITIAL.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.

    ls_return-message = TEXT-032.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.

*    MOVE:
*       TEXT-032 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  IF ls_lubrificantes-ordem IS INITIAL..
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.

    ls_return-message = TEXT-034.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.

*    MOVE:
*        TEXT-034 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.

  IF ls_lubrificantes-namostra IS INITIAL AND ls_lubrificantes-code EQ '0040'.
    CONCATENATE TEXT-027 lv_tabix INTO wa_ret-tipo SEPARATED BY space.

    ls_return-message = TEXT-096.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.

*    MOVE:
*        TEXT-096 TO WA_RET-MESSAGE.
*    APPEND WA_RET TO IT_RET.
    p_erro = 'X'.
  ENDIF.


ENDFORM.                    " ZF_CHECK_TBOLEO
*&---------------------------------------------------------------------*
*&      Form  ZF_ENCE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LUBRIFICANTES_ORDEM  text
*----------------------------------------------------------------------*
FORM zf_ence_order  USING p_ordem TYPE aufk-aufnr.
  DATA: tl_methods      TYPE bapi_alm_order_method OCCURS 0 WITH HEADER LINE,
        tl_header       TYPE bapi_alm_order_headers_i OCCURS 0 WITH HEADER LINE,
        tl_header_up    TYPE bapi_alm_order_headers_up OCCURS 0 WITH HEADER LINE,
        tl_partner      TYPE bapi_alm_order_partn_mul OCCURS 0 WITH HEADER LINE,
        tl_partner_up   TYPE bapi_alm_order_partn_mul_up OCCURS 0 WITH HEADER LINE,
        tl_operation    TYPE bapi_alm_order_operation OCCURS 0 WITH HEADER LINE,
        tl_operation_up TYPE bapi_alm_order_operation_up OCCURS 0 WITH HEADER LINE,
        tl_numbers      TYPE bapi_alm_numbers OCCURS 0 WITH HEADER LINE,
        tl_return       LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        tl_return2      LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        lv_msg          TYPE c LENGTH 220.

  tl_methods-refnumber = 1.
  tl_methods-objecttype = 'HEADER'.
  tl_methods-method = 'TECHNICALCOMPLETE '.
  tl_methods-objectkey = p_ordem.
  APPEND tl_methods.

  tl_methods-objecttype = ''.
  tl_methods-method = 'SAVE'.
  APPEND tl_methods.

  tl_header-orderid =  p_ordem.
  APPEND tl_header.
  "*---> 28/06/2023 - Migração S4 - LO -> não foi usada lista de objeto e material
  CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN' "#EC CI_USAGE_OK[2669857]
    TABLES                               "#EC CI_USAGE_OK[2438131]
      it_methods      = tl_methods
      it_header       = tl_header
      it_header_up    = tl_header_up
      it_partner      = tl_partner
      it_partner_up   = tl_partner_up
      it_operation    = tl_operation
      it_operation_up = tl_operation_up
      return          = tl_return
      et_numbers      = tl_numbers.

  READ TABLE tl_return INTO tl_return WITH KEY type   = 'E'.
*                                               ID     = 'IW'
*                                               NUMBER = '080'.
  IF sy-subrc IS INITIAL.
    APPEND tl_return TO it_return.

*    CONCATENATE 'Não foi possível encerrar a ordem ' P_ORDEM INTO LV_MSG SEPARATED BY SPACE.
*    MESSAGE LV_MSG TYPE 'I'.


  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*    IMPORTING
*      RETURN = TL_RETURN2.
  ENDIF.
ENDFORM.                    " ZF_ENCE_ORDER
*&---------------------------------------------------------------------*
*&      Form  ZF_SALDO_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_DT_PONTO  text
*      -->P_WA_EQUI_SWERK  text
*      -->P_VL_MATNR  text
*      -->P_WA_COMB_POSTO  text
*      -->P_WA_EQUI_DAUFN  text
*      -->P_WA_COMB_QUANT  text
*      <--P_VL_SALDO  text
*----------------------------------------------------------------------*
FORM zf_saldo_mat USING p_sdate TYPE sy-datum
                        p_werks TYPE t001l-werks
                        p_matnr TYPE mara-matnr
                        p_posto TYPE t370fld_stn-station
                        p_ordem TYPE v_equi-daufn
                        p_quant TYPE zftpme_lubrificante-readg
               CHANGING c_error TYPE abap_bool.

  DATA: ls_header  TYPE bapi2017_gm_head_01,
        ls_code    TYPE bapi2017_gm_code,
        ls_item    TYPE bapi2017_gm_item_create,
        lt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_return  TYPE STANDARD TABLE OF bapiret2,
        lw_return  TYPE bapiret2,
        ls_testrun TYPE STANDARD TABLE OF bapi2017_gm_gen-testrun,
        lw_testrun TYPE bapi2017_gm_gen,
*        LS_RETURN TYPE BAPIRET2,
        lv_mater   TYPE bapi2017_gm_head_ret-mat_doc,
        lv_matkl   TYPE mara-matkl.

  IF p_sdate IS NOT INITIAL AND
     p_werks IS NOT INITIAL AND
     p_matnr IS NOT INITIAL.

    TRY.
*        CLEAR: C_SALDO.

        ls_header-pstng_date = v_dt_ponto.
        ls_header-doc_date   = v_dt_ponto.

        ls_code-gm_code      = '03'.

        "*---> 28/06/2023 - Migração S4 - LO
*        ls_item-material =     |{ p_matnr ALPHA = IN  }|.

*        DATA(v_mat) = |{ p_matnr ALPHA = IN  }|.
*        DATA(v_len) = strlen( v_mat ).
*
*        IF v_len > 18.
*          ls_item-material_long = v_mat.
*        ELSE.
*          ls_item-material      = v_mat.
*        ENDIF.
*        CLEAR v_mat.
        "*<--- 28/06/2023 - Migração S4 - LO

        ls_item-orderid =      |{ p_ordem ALPHA = IN  }|.
        ls_header-ref_doc_no = |{ p_ordem ALPHA = IN  }|.

        "*<--- 28/06/2023 - Migração S4 - LO
*        SELECT SINGLE matkl
*          FROM  mara
*          INTO lv_matkl
*         WHERE matnr = ls_item-material.

*        IF ls_item-material IS INITIAL.

        CLEAR: vg_matnr_aux.
        FREE: rg_matnr.

        IF p_matnr IS NOT INITIAL.

          ls_item-material = |{ p_matnr ALPHA = IN }|.
          vg_matnr_aux = p_matnr.
          vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ p_matnr ALPHA = IN }| ) TO rg_matnr.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ vg_matnr_aux }| ) TO rg_matnr.


          SELECT SINGLE matkl
            FROM  mara
            INTO lv_matkl
           WHERE matnr IN rg_matnr.
        ENDIF.
*        ELSE.
*          SELECT SINGLE matkl
*            FROM  mara
*            INTO lv_matkl
*           WHERE matnr = ls_item-material.
*        ENDIF.
        "*<--- 28/06/2023 - Migração S4 - LO

        SELECT SINGLE saknr
          FROM zmmt0039
          INTO ls_item-gl_account
         WHERE matkl = lv_matkl.

        SELECT SINGLE vornr
          FROM afvc AS a
         INNER JOIN afko AS b ON a~aufpl = b~aufpl
          INTO ls_item-activity
         WHERE b~aufnr = ls_item-orderid.

        ls_item-plant        = p_werks.
        ls_item-move_type    = '261'.
        ls_item-entry_qnt    = p_quant.

        SELECT SINGLE storage
        FROM t370fld_stn
        INTO ls_item-stge_loc
        WHERE station = p_posto.

        APPEND ls_item TO lt_item.
        CLEAR ls_item.

        lw_testrun-testrun = ' '.
        APPEND lw_testrun TO ls_testrun.

        "*---> 28/06/2023 - Migração S4 - LO
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            goodsmvt_header  = ls_header
            goodsmvt_code    = ls_code
          IMPORTING
            materialdocument = lv_mater
          TABLES
            goodsmvt_item    = lt_item
            return           = lt_return.

        CLEAR: ls_header, ls_code, lt_item[].

        IF lv_mater IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          wa_input-doc_material = lv_mater.

          CONCATENATE TEXT-053 lv_mater TEXT-042 INTO ls_return-message SEPARATED BY space.

          PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                      CHANGING et_return.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          LOOP AT  lt_return INTO lw_return.
            PERFORM zf_ins_mensagem USING c_e lw_return-number lw_return-id space space lw_return-message space CHANGING et_return.
            p_erro = 'X'.
            EXIT.
          ENDLOOP.
        ENDIF.

      CATCH cx_root.
        ls_return-message = |Erro ao consultar saldo do material ({ p_matnr }).|.
        PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space
                            CHANGING et_return.
        c_error = abap_true.
    ENDTRY.
  ENDIF.
ENDFORM.                    "zf_saldo_mat
*&---------------------------------------------------------------------*
*&      Form  ZF_TRANSFERE_TANQUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_transfere_tanque .

  DATA: ls_header  TYPE bapi2017_gm_head_01,
        ls_code    TYPE bapi2017_gm_code,
        ls_item    TYPE bapi2017_gm_item_create,
        lt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        lt_return  TYPE STANDARD TABLE OF bapiret2,
        lw_return  TYPE bapiret2,
        ls_testrun TYPE STANDARD TABLE OF bapi2017_gm_gen-testrun,
        lw_testrun TYPE bapi2017_gm_gen,
        lv_matkl   TYPE mara-matkl,
        lv_year    TYPE bapi2017_gm_head_ret-doc_year,
        lv_mater   TYPE bapi2017_gm_head_ret-mat_doc.

  DATA: ls_t001k TYPE  t001k,
        lv_data  TYPE sy-datum,
        lv_hora  TYPE sy-uzeit.

*   verificar se período está aberto

  IF wa_input-i_cod_planta_transa IS NOT INITIAL.
    wa_equi-swerk = |{ wa_input-i_cod_planta_transa ALPHA = OUT }|.
  ELSE.
    ls_return-message = |O campo I_COD_PLANTA_TRANSA deve ser informamdo!|.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  IF wa_input-dt_inicio_mov IS INITIAL.
    ls_return-message = |O campo DT_INICIO_MOV deve ser informamdo!|.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  IF wa_input-s_cod_combust IS INITIAL AND wa_input-s_cod_produto IS INITIAL.
    ls_return-message = |O campo S_COD_COMBUST ou S_COD_PRODUTO deve ser informamdo!|.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  IF wa_input-f_qtde_abast IS INITIAL AND wa_input-f_qtde_produto IS INITIAL.
    ls_return-message = |O campo F_QTDE_ABAST ou F_QTDE_PRODUTO deve ser informamdo!|.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  CALL FUNCTION 'AIP01_PLANT_DETERMINE'
    EXPORTING
      i_werks  = wa_equi-swerk
    IMPORTING
      es_t001k = ls_t001k
    EXCEPTIONS
      OTHERS   = 1.

  CALL FUNCTION 'CONVERT_DATE_INPUT'
    EXPORTING
      input                     = wa_input-dt_inicio_mov
    IMPORTING
      output                    = v_dt_ponto
    EXCEPTIONS
      plausibility_check_failed = 1
      wrong_format_in_input     = 2.

  IF sy-subrc IS NOT INITIAL.
    v_dt_ponto = wa_input-dt_inicio_mov.
  ENDIF.

  ls_header-pstng_date = v_dt_ponto.
  ls_header-doc_date   = v_dt_ponto.
*
  CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
    EXPORTING
      p_data_ent     = v_dt_ponto
      p_bukrs        = ls_t001k-bukrs
      p_val_fi       = 'X'
      p_val_mm       = 'X'
    EXCEPTIONS
      data_fi_mm_nao = 1
      OTHERS         = 2.
*
  IF sy-subrc IS NOT INITIAL.
    ls_return-message = |A data informada esta em um período bloqueado para a empresa { ls_t001k-bukrs }.|.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
  ENDIF.

* verificar qual é o material
  CLEAR: vg_matnr_aux.
  IF NOT wa_input-s_cod_combust IS INITIAL.
    vg_matnr_aux = wa_input-s_cod_combust.
  ELSE.
    vg_matnr_aux = wa_input-s_cod_produto.
  ENDIF.
  vg_matnr_aux = |{ vg_matnr_aux ALPHA = IN }|.
  ls_item-material = vg_matnr_aux.

  ls_item-plant        = wa_equi-swerk.
  ls_item-move_plant   = wa_equi-swerk.
  ls_item-move_type    = '311'.

  IF NOT wa_input-f_qtde_abast IS INITIAL.
    ls_item-entry_qnt    = wa_input-f_qtde_abast.
  ELSE.
    ls_item-entry_qnt    = wa_input-f_qtde_produto.
  ENDIF.

  IF wa_input-s_id_tanque_origem IS NOT INITIAL AND wa_input-s_id_tanque_destino IS NOT INITIAL.

    SELECT SINGLE storage
      FROM t370fld_stn
        INTO ls_item-stge_loc
      WHERE station EQ wa_input-s_id_tanque_origem.

*    IF WA_INPUT-I_TIPO_REGISTRO EQ C_3.
*
*      SELECT SINGLE *
*        FROM SETLEAF
*        INTO @DATA(WA_SETLEAF)
*       WHERE SETNAME EQ 'AFERIR_PM_IONICS'
*         AND VALFROM EQ @WA_INPUT-S_ID_TANQUE_ORIGEM.
*
*      IF SY-SUBRC IS INITIAL.
*        SELECT SINGLE *
*          FROM SETLINET
*          INTO @DATA(WT_SETLINET)
*         WHERE SETNAME EQ @WA_SETLEAF-SETNAME
*            AND LINEID EQ @WA_SETLEAF-LINEID.
*      ENDIF.
*
*      LS_ITEM-MOVE_STLOC = CONV #( WT_SETLINET-DESCRIPT ).
*
*    ELSE.
    SELECT SINGLE storage
      FROM t370fld_stn
        INTO ls_item-move_stloc
      WHERE station EQ wa_input-s_id_tanque_destino.
*    ENDIF.

  ELSE.
    ls_return-message = |O Tanque Origem e o Tanque Destino são obrigatórios para realizar a Transferência!|.
    PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message  space CHANGING et_return.
    p_erro = abap_true.
    EXIT.
  ENDIF.

  ls_code = '06'.

  wa_input-ib_ordem = |{ wa_input-ib_ordem ALPHA = IN }|.

  SELECT SINGLE vornr
    FROM afvc AS a
   INNER JOIN afko AS b ON a~aufpl = b~aufpl
    INTO ls_item-activity
   WHERE b~aufnr = wa_input-ib_ordem.

  APPEND ls_item TO lt_item.
  CLEAR ls_item.

  lw_testrun-testrun = ' '.
  APPEND lw_testrun TO ls_testrun.

  "*---> 28/06/2023 - Migração S4 - LO
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = ls_header
      goodsmvt_code    = ls_code
    IMPORTING
      materialdocument = lv_mater
      matdocumentyear  = lv_year
    TABLES
      goodsmvt_item    = lt_item
      return           = lt_return.

  CLEAR: ls_header, ls_code, lt_item[].

  IF lv_mater IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    wa_input-doc_material = lv_mater.

    CONCATENATE TEXT-053 lv_mater TEXT-042 INTO ls_return-message SEPARATED BY space.

    PERFORM zf_inserir_mensagem USING c_s 0 space space space  space
                                CHANGING et_return.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT  lt_return INTO lw_return.
      PERFORM zf_ins_mensagem USING c_e lw_return-number lw_return-id space space lw_return-message space CHANGING et_return.
      p_erro = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.


  LOOP AT it_retmed INTO wa_retmed.
    CLEAR wa_log.
    PERFORM gera_id CHANGING wa_log-id.
    wa_log-hora       = lv_hora.
    wa_log-data       = lv_data.
    wa_log-usuario    = sy-uname.
    wa_log-mensagem   = wa_retmed-message.
*
    INSERT INTO zlogapont VALUES wa_log.
  ENDLOOP.
*** Fim Grava log
  CLEAR: wa_saida1, it_saida1[].
  CLEAR: wa_saida2, it_saida2[].
  CLEAR: wa_comb, v_erro, v_erroo, v_erroh, v_dt_ponto,
         v_hr_ponto, v_equip, wa_equi, v_equnr,
         v_hr_pto, v_pointo, v_itemo, v_medano, v_medato,
         v_unido, v_pointh, v_itemh, v_medanh, v_medath,
         v_unidh, it_log[], v_nbaixa, v_n_form.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DR  text
*----------------------------------------------------------------------*
FORM convert_data CHANGING vl_data.

  REPLACE ALL OCCURRENCES OF '/' IN vl_data WITH ''.

  TRY.
      cl_abap_datfm=>conv_date_ext_to_int( EXPORTING im_datext = vl_data IMPORTING ex_datint = DATA(data_convertida) ).
      vl_data = data_convertida.
    CATCH cx_abap_datfm_no_date.
    CATCH cx_abap_datfm_invalid_date.
    CATCH cx_abap_datfm_format_unknown.
    CATCH cx_abap_datfm_ambiguous.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estorno USING  id_saaf     TYPE ze_i_cod_transa_saaf
                    id_saaf_org TYPE ze_i_cod_transa_saaf.

  DATA: v_dt_ponto  TYPE sy-datum,
        vg_bukrs    TYPE j_1bbranch-bukrs,
        vg_werks    TYPE j_1bbranch-branch,
        e_status(1),
        e_messa(64).


  DATA return TYPE TABLE OF bapiret2.
  DATA wa_saaf TYPE ztpm_imp_do_saaf.

  DATA: mblnr_           TYPE mblnr,
        mjahr_           TYPE mjahr,
        goodsmvt_headret TYPE bapi2017_gm_head_ret,
        _atnam           TYPE atnam.

  DATA: mdocm_ TYPE RANGE OF imrc_mdocm,
        imrg_  TYPE imrg.
  DATA: _equnr TYPE equnr.

  DATA : t_canc_req TYPE TABLE OF imrg_mdocm.


  IF id_saaf_org IS NOT INITIAL.
    SELECT SINGLE *
      FROM ztpm_imp_do_saaf
      INTO wa_saaf
      WHERE i_cod_transa_saaf EQ id_saaf_org.
  ELSE.
    SELECT SINGLE *
    FROM ztpm_imp_do_saaf
    INTO wa_saaf
    WHERE i_cod_transa_saaf EQ id_saaf.
  ENDIF.

  CHECK wa_saaf IS NOT INITIAL.

  v_dt_ponto = wa_saaf-dt_inicio_mov.

  CHECK sy-subrc IS INITIAL.

  mblnr_ = wa_saaf-doc_material.
  mjahr_ = sy-datum(4).

  vg_werks = |{ wa_saaf-i_cod_planta_transa ALPHA = IN }|.

  SELECT SINGLE bukrs
    INTO vg_bukrs
    FROM j_1bbranch
  WHERE branch EQ vg_werks.

  IF sy-subrc = 0.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = vg_bukrs
        i_data   = v_dt_ponto
      IMPORTING
        e_status = e_status
        e_messa  = e_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF  e_status = 'E'.
      MOVE e_messa TO ls_return-message.
      PERFORM zf_inserir_mensagem USING  c_e 999 space space ls_return-message  space
                     CHANGING et_return.
      p_erro = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF gt_msg IS INITIAL.

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument    = mblnr_
        matdocumentyear     = mjahr_
        goodsmvt_pstng_date = sy-datum
      IMPORTING
        goodsmvt_headret    = goodsmvt_headret
      TABLES
        return              = return.

    IF line_exists( return[ type = 'E' ] ).
      ls_return-message = return[ type = 'E' ]-message.
      PERFORM zf_inserir_mensagem USING c_e 02 space space space space CHANGING et_return.
      PERFORM log.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      IF goodsmvt_headret-mat_doc IS NOT INITIAL.
        UPDATE ztpm_imp_do_saaf
        SET doc_estorno = goodsmvt_headret-mat_doc
        WHERE i_cod_transa_saaf EQ wa_saaf-i_cod_transa_saaf
          AND doc_material      EQ mblnr_.
      ENDIF.

      ls_return-message = |Doc.Material { mblnr_ } Estornado por { goodsmvt_headret-mat_doc }|.
      PERFORM zf_inserir_mensagem USING c_s 01 space space space space CHANGING et_return.
      PERFORM log.

    ENDIF.

    CHECK wa_saaf-i_tipo_registro NE 2.

****************************************************************************************************

    v_equip  = wa_saaf-s_cod_equipamento.

    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        i_equnr    = v_equip
      TABLES
        et_return1 = it_return
        et_diimpt  = it_dimpt.

    _atnam = 'COMBUSTIVEL'.

    IF wa_saaf-s_cod_compartimento IS NOT INITIAL.
      v_locas = wa_saaf-s_cod_compartimento.

      TRY.
          _atnam = it_dimpt[ locas = v_locas ]-atnam.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ENDIF.

    mdocm_ = VALUE #( FOR ls IN it_dimpt WHERE ( inact EQ abap_false AND
                                               ( atnam EQ 'ODOMETRO' OR
                                                 atnam EQ 'HORIMETRO' OR
                                                 atnam EQ _atnam ) )
                                                 (
                                                    option = 'EQ'
                                                    sign = 'I'
                                                    low = ls-point
                                                 )
                    ).

    SELECT mdocm
      FROM imrg
      INTO TABLE t_canc_req
      WHERE point IN mdocm_
        AND idate EQ wa_saaf-dt_inicio_mov
        AND itime EQ wa_saaf-s_hora_inicio_mov(4).

    CHECK t_canc_req IS NOT INITIAL.

    CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
      EXPORTING
        messages_allowed       = abap_false
      TABLES
        cancel_requests        = t_canc_req
      EXCEPTIONS
        no_authority           = 1
        foreign_lock_occured   = 2
        system_failure_occured = 3
        recursiveness_found    = 4
        error_message          = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log .

  PERFORM gera_id CHANGING wa_log-id.

  wa_log = VALUE #(
                    id         = wa_log-id
                    hora       = sy-uzeit
                    data       = sy-datum
                    usuario    = sy-uname
                    mensagem   = ls_return-message
                  ).

  INSERT INTO zlogapont VALUES wa_log.
  CLEAR wa_log.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTOR_DOCUMENTO_MEDIÇÃO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estor_documento_medicao .


  IF t_doc_med IS NOT INITIAL.
    CALL FUNCTION 'MEASUREM_DOCUM_CANCEL_ARRAY'
      EXPORTING
        messages_allowed       = abap_false
      TABLES
        cancel_requests        = t_doc_med
      EXCEPTIONS
        no_authority           = 1
        foreign_lock_occured   = 2
        system_failure_occured = 3
        recursiveness_found    = 4
        error_message          = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    LOOP AT t_doc_med ASSIGNING FIELD-SYMBOL(<w_doc>).
      CONCATENATE 'Documento medição estornado com sucesso' <w_doc> INTO ls_return-message SEPARATED BY space.
      PERFORM zf_inserir_mensagem USING c_e 999 space space ls_return-message space CHANGING et_return.
    ENDLOOP.

  ENDIF.

ENDFORM.
