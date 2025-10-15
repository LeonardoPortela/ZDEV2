*----------------------------------------------------------------------*
***INCLUDE LZLES0003F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONTARAZAO_EXIGE_CCUSTO
*&---------------------------------------------------------------------*
FORM check_contarazao_exige_ccusto USING  p_conta_razao
                                          p_tipo_conta
                                          p_chave_lanc
                                CHANGING  p_exige_ccusto
                                          p_exige_material.

  STATICS: lqbr_contadebito    TYPE saknr,
           lqbr_contacredito   TYPE saknr,
           lf_ccusto_debito,
           lf_ccusto_credito,
           lf_material_debito,
           lf_material_credito.

  FIELD-SYMBOLS: <conta-razao>    TYPE c,
                 <exige_ccusto>   TYPE c,
                 <exige_material> TYPE c.

  DATA: wa_cskb TYPE cskb.

* Limpa retorno
  CLEAR: lqbr_contadebito, lqbr_contacredito, p_exige_ccusto, p_exige_material, lf_ccusto_debito, lf_ccusto_credito, lf_material_debito, lf_material_credito.

  CHECK p_chave_lanc NE '21' AND p_chave_lanc NE '31' AND p_chave_lanc NE '29' AND p_chave_lanc NE '39'.

* Verifica quebra de conta de débito ou credito
  IF p_tipo_conta = 'D'.
    IF lqbr_contadebito <> p_conta_razao.
      lqbr_contadebito = p_conta_razao.
      CLEAR lf_ccusto_debito.
      ASSIGN: lqbr_contadebito   TO <conta-razao>,
              lf_ccusto_debito   TO <exige_ccusto>,
              lf_material_debito TO <exige_material>.
    ENDIF.
  ELSEIF p_tipo_conta = 'C'.

    IF lqbr_contacredito <> p_conta_razao.
      lqbr_contacredito = p_conta_razao.
      CLEAR lf_ccusto_credito.
      ASSIGN: lqbr_contacredito   TO <conta-razao>,
              lf_ccusto_credito   TO <exige_ccusto>,
              lf_material_credito TO <exige_material>.
    ENDIF.
  ENDIF.

* Verifica acesso a base de dados
  IF <conta-razao> IS ASSIGNED AND <exige_ccusto> IS ASSIGNED AND <exige_material> IS ASSIGNED.

    SELECT SINGLE * INTO wa_cskb "#EC CI_DB_OPERATION_OK[2389136]
      FROM cskb
     WHERE kokrs = 'MAGI'
       AND kstar = <conta-razao>
       AND datbi >= sy-datum.

    IF sy-subrc IS INITIAL.
      CLEAR: <exige_ccusto>, <exige_material>.
      CASE wa_cskb-katyp.
        WHEN '01'.
          "Categoria de classe de custo para CENTRO DE CUSTO
          <exige_ccusto>   = 'X'.
        WHEN '11' OR '12'.
          "Categoria de classe de custo para MATERIAL
          <exige_material> = 'X'.
      ENDCASE.
    ELSE.
      CLEAR: <exige_ccusto>, <exige_material>.
    ENDIF.

  ENDIF.

* Retorna condição se a conta razão exige centro de custo
  IF p_tipo_conta = 'D'.
    p_exige_ccusto   = lf_ccusto_debito.
    p_exige_material = lf_material_debito.
  ELSEIF p_tipo_conta = 'C'.
    p_exige_ccusto   = lf_ccusto_credito.
    p_exige_material = lf_material_credito.
  ENDIF.

ENDFORM.                    " CHECK_CONTARAZAO_EXIGE_CCUSTO

*&---------------------------------------------------------------------*
*&      Form  OBTEM_CENTRO_CUSTO_CONTARAZAO
*&---------------------------------------------------------------------*
FORM obtem_centro_custo_contarazao USING  p_codtrp
                                          p_tipo_conta
                                          p_conta_razao
                                CHANGING  p_kostl.

  STATICS: lqbr_codtrp       TYPE zcodtrp,
           lqbr_contadebito  TYPE saknr,
           lqbr_contacredito TYPE saknr,
           lc_bukrs          TYPE bukrs,
           lc_gsber          TYPE gsber,
           lc_kostl_debito   TYPE kostl,
           lc_kostl_credito  TYPE kostl.

  DATA:    lc_saknr             TYPE saknr.
  FIELD-SYMBOLS: <conta-razao1>  TYPE c,
                 <centro-custo1> TYPE c.
* Limpa retorno
  CLEAR p_kostl.

  CLEAR:   lqbr_contadebito,
           lqbr_codtrp,
           lqbr_contacredito,
           lc_bukrs,
           lc_gsber,
           lc_kostl_debito,
           lc_kostl_credito.

* Verifica quebra da transportadora
  IF  p_codtrp <> lqbr_codtrp.
    lqbr_codtrp = p_codtrp.
    lc_gsber = p_codtrp+6.
    SELECT bukrs
      INTO lc_bukrs
      FROM j_1bbranch
        UP TO 1 ROWS
     WHERE branch = lc_gsber.
    ENDSELECT.
  ENDIF.

* Verifica quebra de conta de débito / crédito
  IF p_tipo_conta = 'D'.
    IF lqbr_contadebito <> p_conta_razao.
      lqbr_contadebito = p_conta_razao.
      ASSIGN: lqbr_contadebito TO <conta-razao1>,
              lc_kostl_debito  TO <centro-custo1>.
    ENDIF.
  ELSEIF p_tipo_conta = 'C'.
    IF lqbr_contacredito <> p_conta_razao.
      lqbr_contacredito = p_conta_razao.
      ASSIGN: lqbr_contacredito TO <conta-razao1>,
              lc_kostl_credito  TO <centro-custo1>.
    ENDIF.
  ENDIF.

* Verifica acesso a base de dados
  IF <conta-razao1> IS ASSIGNED AND <centro-custo1> IS ASSIGNED.
    SELECT MAX( saknr ) kostl
      INTO (lc_saknr, <centro-custo1>)
      FROM zlest0033
     WHERE bukrs = lc_bukrs
       AND gsber = lc_gsber
       AND (  saknr = <conta-razao1> OR saknr = space )
     GROUP BY saknr kostl.
    ENDSELECT.
  ENDIF.

* Retorna o centro de custo se localizado
  IF p_tipo_conta = 'D'.
    p_kostl = lc_kostl_debito.
  ELSEIF p_tipo_conta = 'C'.
    p_kostl = lc_kostl_credito.
  ENDIF.
  IF <conta-razao1> IS ASSIGNED.
    UNASSIGN <conta-razao1>.
  ENDIF.
  IF <centro-custo1> IS ASSIGNED.
    UNASSIGN <centro-custo1>.
  ENDIF.
ENDFORM.                    " OBTEM_CENTRO_CUSTO_CONTARAZAO
