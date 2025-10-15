"Name: \PR:RFFOBR_D\FO:FILL_DETAILS_ITAU\SE:END\EI
ENHANCEMENT 0 Z_ITAU_JUROS.
* Ini - Ajuste no arquivo de remessa do ITAU - #124586 - RJF
IF j_1bdmeya-a09 = '000' OR j_1bdmeya-a09 IS INITIAL.
  j_1bdmeya-a09  = '109'. " N. Cart
ENDIF.

IF j_1bdmeya-a11 = '0' OR j_1bdmeya-a11 IS INITIAL.
  j_1bdmeya-a11  = 'I'. " Cart
ENDIF.

IF j_1bdmeya-a12 = '00' OR j_1bdmeya-a12 IS INITIAL.
  j_1bdmeya-a12  = '01'. " Ident. Ocorrência
ENDIF.

j_1bdmeya-a13 = regup-belnr. " Nº duplicata- doc
j_1bdmeya-a17 = '00000'.     "Agency or other info,
* Fim - Ajuste no arquivo de remessa do ITAU - #124586 - RJF

DATA: vl_valor    TYPE zde003,
      vl_v1       TYPE p LENGTH 13 DECIMALS 4,
      vl_v2       TYPE p LENGTH 13 DECIMALS 4,
      vl_v3       TYPE p LENGTH 13 DECIMALS 2,
      vl_data     TYPE sy-datum,
      vl_day      TYPE n LENGTH 2,
      lv_v_titulo TYPE qbshh.

CLEAR vl_valor.
CALL FUNCTION 'Z_BUSCA_JUROS_MULTA_BOLETO'
  EXPORTING
    i_belnr = regup-belnr
    i_bukrs = regup-bukrs
  IMPORTING
    e_juros = vl_valor. "Valor Juros em porcentagem

IF vl_valor > 0.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = sy-datum
    IMPORTING
      last_day_of_month = vl_data
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  vl_day = vl_data+6(2)."Último dia do mês

  PERFORM amount_check CHANGING lv_v_titulo."Valor do Titulo

  vl_v1 = vl_valor / 12.
  vl_v2 = ( lv_v_titulo * vl_v1 ) / 100.
  vl_v3 = vl_v2 / vl_day."Valor diário do Juros
  vl_v3 = vl_v3 / 100.
***      vl_valor =  (  ) / vl_day.

  MOVE vl_v3 TO j_1bdmeya-a23.

  PERFORM for_curr_check USING regup-waers t001-waers
                   CHANGING j_1bdmeya-a23.
ENDIF.
ENDENHANCEMENT.
