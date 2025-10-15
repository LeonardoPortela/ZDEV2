FUNCTION ZF_CALCULA_INTERVALO_DATA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATE1) TYPE  SY-DATUM OPTIONAL
*"     VALUE(I_DATE2) TYPE  SY-DATUM OPTIONAL
*"     VALUE(I_DATE3) TYPE  SY-DATUM OPTIONAL
*"     VALUE(I_DATE4) TYPE  SY-DATUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_INTERVALO_APROPRIACAO) TYPE  P
*"     REFERENCE(E_INTERVALO_VIGENCIA) TYPE  P
*"     REFERENCE(E_INTERVALO_PARCELAS) TYPE  P
*"----------------------------------------------------------------------


  DATA:
    v_data_inicio_apropriacao  TYPE sy-datum,
    v_data_fim_apropriacao     TYPE sy-datum,
    "
    v_data_inicio_apropriacao2 TYPE sy-datum,
    v_data_fim_apropriacao2    TYPE sy-datum,
    "
    v_data_inicio_vigencia     TYPE sy-datum,
    v_data_fim_vigencia        TYPE sy-datum,
    v_competencia              TYPE c LENGTH 6,
    return_status              TYPE c.

  DATA: at_months TYPE tfmatage.

  v_data_inicio_apropriacao = i_date1.
  v_data_fim_apropriacao    = i_date2.
  v_data_inicio_vigencia    = i_date3.
  v_data_fim_vigencia       = i_date4.

*  Calcular diferença entre a data inicial/final digitada.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from    = v_data_inicio_apropriacao
      i_date_to      = v_data_fim_apropriacao
      i_flg_separate = ' '
    IMPORTING
      e_months       = at_months.

  IF ( at_months IS INITIAL ).
    e_intervalo_apropriacao = 1.
  ELSE.
    e_intervalo_apropriacao = at_months.
  ENDIF.

*  Calcular diferença entre a data inicial/final de vigência do cadastro.
  "ALRS
  CONCATENATE v_data_fim_vigencia+4(2)  v_data_fim_vigencia+0(4)  INTO v_competencia.
  PERFORM valida_competencia USING v_competencia
                                CHANGING return_status
                                         v_data_inicio_apropriacao2
                                         v_data_fim_apropriacao2.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from    = v_data_inicio_vigencia
      i_date_to      = v_data_inicio_apropriacao2 "V_DATA_FIM_VIGENCIA
      i_flg_separate = ' '
    IMPORTING
      e_months       = at_months.

  e_intervalo_vigencia = at_months.

  IF ( at_months IS INITIAL ).
    IF ( v_data_inicio_vigencia+6(2) <= 15 ).
      e_intervalo_vigencia = 1.
    ELSE.
      e_intervalo_vigencia = 0. "ALRS
    ENDIF.
  ELSE.
    e_intervalo_vigencia = at_months.

    IF ( v_data_inicio_vigencia+6(2) <= 15 ).
      e_intervalo_vigencia = at_months + 1.
    ENDIF.

  ENDIF.

*  Calcular intervalo entre a data inicial digitada, e a data inicial da vigência do cadastro,
*  para assim saber qual sera a parcela da sequência, exemplo:
*  Data inicial de vigência = 01.01.2015 |
*  Data inicial digitada    = 01.02.2015 -- > Diferença entre as datas é de 1 mês
*  Logo, começamos na parcela 2.
*  -
*  Abaixo, fazemos exatamente esta tratativa, caso não houver diferença, começamos
*  na parcela 1.

*    IF V_DATA_INICIO_VIGENCIA+4(2) EQ V_DATA_INICIO_APROPRIACAO+4(2) AND V_DATA_INICIO_VIGENCIA+6(2) > 15.
*
*      E_INTERVALO_PARCELAS = 0.
*
*    ELSE.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from    = v_data_inicio_vigencia
      i_date_to      = v_data_inicio_apropriacao
      i_flg_separate = ' '
    IMPORTING
      e_months       = at_months.

  IF ( at_months IS INITIAL ).
    IF ( v_data_inicio_vigencia+6(2) <= 15 ).
      e_intervalo_parcelas = 1.
    ELSE.
      e_intervalo_parcelas = 0. "ALRS
    ENDIF.
  ELSE.
*      IF ( V_DATA_INICIO_VIGENCIA+6(2) > 15 ).
    e_intervalo_parcelas = at_months.
*      ELSE.
*        E_INTERVALO_PARCELAS = AT_MONTHS + 1.
*      ENDIF.

    IF ( v_data_inicio_vigencia+6(2) <= 15 ).
      e_intervalo_parcelas = at_months + 1.
    ENDIF.

  ENDIF.

*    ENDIF.


ENDFUNCTION.
