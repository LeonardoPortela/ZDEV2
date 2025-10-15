*----------------------------------------------------------------------*
***INCLUDE LZGSD_SELOS_SOJA_SAPF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CALCULAR_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_calcular_lote CHANGING p_lote.

  FREE: p_lote.

  l_ciclo  = w_zpmt0054-ciclo_sem - 1.

  CONCATENATE l_data_ref(4) '1231'
         INTO l_dt_fim.

*-----------------------------
* nro semana para o dia
*-----------------------------
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = l_data_ref
    IMPORTING
      week         = l_week
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

*-----------------------------
* nro semana para ultimo dia do ano
*-----------------------------
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = l_dt_fim
    IMPORTING
      week         = l_week_fim
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  l_semana_atu = l_week+4(2).
  l_ano_atu    = l_week+2(2).
  l_semana_fim = l_week_fim+4(2).
  l_ano_fim    = l_week_fim+2(2).

*-----------------------------
* se for primeiro cadastro, seta lote inicial
*-----------------------------
  IF w_zpmt0054-lote IS INITIAL.
    l_lote01   = l_semana_atu.
    l_lote02   = l_semana_atu + l_ciclo.
    l_lote_ano = l_data_ref+2(2).

    CONCATENATE l_ano_atu  l_lote02
           INTO l_chave01.
    CONCATENATE l_ano_fim  l_semana_fim
           INTO l_chave02.

    IF l_chave01 > l_chave02.
      l_lote02   = l_lote02   - l_semana_fim + l_ciclo.
      l_lote_ano = l_lote_ano + 1.
    ENDIF.

    CONCATENATE w_zpmt0054-id_lote
                l_lote01
                l_lote02
                l_lote_ano
           INTO p_lote.
    EXIT.
  ENDIF.

*-----------------------------
* lote permanece o mesmo
*-----------------------------
  l_lote01   = w_zpmt0054-lote+2(2).
  l_lote02   = w_zpmt0054-lote+4(2).
  l_lote_ano = w_zpmt0054-lote+6(2).

  CONCATENATE l_lote_ano l_lote02
         INTO l_chave01.
  CONCATENATE l_lote_ano l_semana_atu
         INTO l_chave02.

  IF l_chave02 <= l_chave01.
    p_lote = w_zpmt0054-lote.
    EXIT.
  ENDIF.

*-----------------------------
* procurar sequencia proximo lote
*-----------------------------
  l_lote01_ant = l_lote01.
  l_lote02_ant = l_lote02.
  l_lote_ano   = l_data_ref+2(2).

  DO.
    l_lote01 = l_lote02 + 1.
    l_lote02 = l_lote01 + l_ciclo.

    CONCATENATE l_lote_ano l_lote02
           INTO l_chave01.
    CONCATENATE l_ano_fim  l_semana_fim
           INTO l_chave02.

    IF l_chave01 > l_chave02.
      l_lote02   = l_lote02   - l_semana_fim + l_ciclo.
      l_lote_ano = l_lote_ano + 1.
    ENDIF.

    CONCATENATE l_lote_ano l_lote02
           INTO l_chave01.
    CONCATENATE l_ano_atu  l_semana_atu
           INTO l_chave02.

    IF l_chave01 > l_chave02.
      EXIT.
    ENDIF.

    l_lote01_ant = l_lote01.
    l_lote02_ant = l_lote02.
  ENDDO.

  CONCATENATE w_zpmt0054-id_lote
              l_lote01
              l_lote02
              l_lote_ano
         INTO p_lote.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CALCULAR_LOTE
*&---------------------------------------------------------------------*
FORM f_calcular_datas CHANGING p_data_fabricacao
                               p_data_validade.

  p_data_fabricacao = l_data_ref        - w_zpmt0054-dia.
  p_data_validade   = p_data_fabricacao + w_zpmt0054-validade.

ENDFORM.
