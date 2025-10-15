*&---------------------------------------------------------------------*
*& Report  ZRD_zppt0036_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zppt0036_exit.

DATA: l_data_str1 TYPE char20,
      l_data_str2 TYPE char20,
      l_data_str3 TYPE char20,
      l_data_str4 TYPE char20,
      l_period1   TYPE char6,
      l_period2   TYPE char6.

FORM f_exit_zppt0036_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zppt0036 TYPE zppt0036_out.

  CLEAR: wl_zppt0036.

  wl_zppt0036-usuario_lanc  = sy-uname.
  wl_zppt0036-data_lanc     = sy-datum.
  wl_zppt0036-hora_lanc     = sy-uzeit.

  MOVE-CORRESPONDING wl_zppt0036 TO p_registro_manter.

ENDFORM.

FORM f_exit_zppt0036_0002    USING p_registro_manter TYPE any
                          CHANGING p_erro.

  DATA: w_zppt0036 TYPE zppt0036_out,
        l_datum    TYPE sy-datum,
        l_uzeit    TYPE sy-uzeit.

  MOVE-CORRESPONDING p_registro_manter TO w_zppt0036.

*-----------------------
* simulacao data
*-----------------------
  SELECT SINGLE *
    INTO @DATA(w_tvarv)
    FROM tvarvc
   WHERE name = 'ZPPT0030_DATA_SIMULA'.
  IF sy-subrc = 0.
    l_datum = w_tvarv-low+6(4) && w_tvarv-low+3(2) && w_tvarv-low(2).
  ELSE.
    l_datum = sy-datum.
  ENDIF.

*-----------------------
* simulacao hora
*-----------------------
  SELECT SINGLE *
    INTO w_tvarv
    FROM tvarvc
   WHERE name = 'ZPPT0030_HORA_SIMULA'.
  IF sy-subrc = 0.
    l_uzeit = w_tvarv-low(2) && w_tvarv-low+3(2) && w_tvarv-low+6(2).
  ELSE.
    l_uzeit = sy-uzeit.
  ENDIF.

  IF w_zppt0036-data_bloq_ini IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Data Inicio de Bloqueio!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zppt0036-hora_bloq_ini IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Hora Inicio de Bloqueio!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zppt0036-data_bloq_fim IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Data Fim de Bloqueio!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF w_zppt0036-hora_bloq_fim IS INITIAL.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Informar Hora Fim de Bloqueio!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  l_data_str1 = w_zppt0036-data_bloq_ini && w_zppt0036-hora_bloq_ini.
  l_data_str2 = w_zppt0036-data_bloq_fim && w_zppt0036-hora_bloq_fim.

  IF l_data_str1 >= l_data_str2.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Periodo Inicial maior que Periodo Final!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------
* historivo
*-----------------------
  SELECT *
    FROM zppt0036
    INTO TABLE @DATA(t_0036).

*-----------------------
* nao incluir antes de data atual
*-----------------------
  l_data_str1 = w_zppt0036-data_bloq_ini && w_zppt0036-hora_bloq_ini.
  l_data_str2 = l_datum                  && l_uzeit.

  IF l_data_str1 < l_data_str2.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não permitido Excluir ou Editar registro'
                          ' já iniciado/encerrado processo de bloqueio!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------
* somente no mes de competencia
*-----------------------
  l_period1 =  w_zppt0036-data_bloq_ini && w_zppt0036-data_bloq_ini+4(02).
  l_period2 =  l_datum                  && l_datum+4(02).

  IF l_period1 <> l_period2.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Inicio do Bloqueio deve estar no mês de Competência!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------
* sobreposicao periodos
*-----------------------
  IF sy-ucomm = 'CHANGE'.
    DELETE t_0036 WHERE data_bloq_ini = w_zppt0036-data_bloq_ini.
  ENDIF.

  LOOP AT t_0036 INTO DATA(w_0036).
    l_data_str1 = w_zppt0036-data_bloq_ini && w_zppt0036-hora_bloq_ini.
    l_data_str2 = w_zppt0036-data_bloq_fim && w_zppt0036-hora_bloq_fim.
    l_data_str3 = w_0036-data_bloq_ini     && w_0036-hora_bloq_ini.
    l_data_str4 = w_0036-data_bloq_fim     && w_0036-hora_bloq_fim.
    IF ( l_data_str1 >= l_data_str3  AND
         l_data_str1 <= l_data_str4 ) OR
       ( l_data_str2 >= l_data_str3  AND
         l_data_str2 <= l_data_str4 ).
      p_erro = abap_true.
      MESSAGE s024(sd) WITH 'Não é Permitido Sobreposição de Periodo!'
                       DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDLOOP.

*-----------------------
* nao pode modificar periodo em processamento
*-----------------------
  l_data_str1 = w_zppt0036-data_bloq_ini && w_zppt0036-hora_bloq_ini.
  l_data_str2 = l_datum                  && l_uzeit.

  IF l_data_str1 <= l_data_str2.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não é Permitido Modificar Periodo em Processamento!'
                     DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0036_0004 CHANGING p_registro_manter TYPE any.

  DATA: w_zppt0036 TYPE zppt0036_out.
  CLEAR w_zppt0036.

  MOVE-CORRESPONDING p_registro_manter  TO w_zppt0036.

  MOVE-CORRESPONDING w_zppt0036 TO p_registro_manter.
ENDFORM.

FORM f_exit_zppt0036_0005 CHANGING p_registro_manter TYPE any.

ENDFORM.

FORM f_exit_zppt0036_0006 USING p_registro_manter TYPE any
                       CHANGING p_erro.

  DATA: w_zppt0036 TYPE zppt0036_out,
        l_datum    TYPE sy-datum,
        l_uzeit    TYPE sy-uzeit.

  MOVE-CORRESPONDING p_registro_manter TO w_zppt0036.

*-----------------------
* simulacao data
*-----------------------
  SELECT SINGLE *
    INTO @DATA(w_tvarv)
    FROM tvarvc
   WHERE name = 'ZPPT0030_DATA_SIMULA'.
  IF sy-subrc = 0.
    l_datum = w_tvarv-low+6(4) && w_tvarv-low+3(2) && w_tvarv-low(2).
  ELSE.
    l_datum = sy-datum.
  ENDIF.

*-----------------------
* simulacao hora
*-----------------------
  SELECT SINGLE *
    INTO w_tvarv
    FROM tvarvc
   WHERE name = 'ZPPT0030_HORA_SIMULA'.
  IF sy-subrc = 0.
    l_uzeit = w_tvarv-low(2) && w_tvarv-low+3(2) && w_tvarv-low+6(2).
  ELSE.
    l_uzeit = sy-uzeit.
  ENDIF.

*-----------------------
* nao pode eliminar  periodo em processamento
*-----------------------
  l_data_str1 = w_zppt0036-data_bloq_ini && w_zppt0036-hora_bloq_ini.
  l_data_str2 = l_datum                  && l_uzeit.

  IF l_data_str1 <= l_data_str2.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não permitido Excluir ou Editar registro'
                          ' já iniciado/encerrado processo de bloqueio!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0036_0008 CHANGING p_col_pos
                                  p_ref_tabname
                                  p_ref_fieldname
                                  p_tabname
                                  p_field
                                  p_scrtext_l
                                  p_outputlen
                                  p_edit
                                  p_sum
                                  p_emphasize
                                  p_just
                                  p_hotspot
                                  p_f4
                                  p_check.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'DATA_BLOQ_INI'.
    p_scrtext_l = 'Data Inicio Bloqueio'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'HORA_BLOQ_INI'.
    p_scrtext_l = 'Hora Inicio Bloqueio'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'DATA_BLOQ_FIM'.
    p_scrtext_l = 'Data Fim Bloqueio'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'HORA_BLOQ_FIM'.
    p_scrtext_l = 'Hora Fim Bloqueio'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'USUARIO_LANC'.
    p_scrtext_l = 'Usuário Lançamento'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'DATA_LANC'.
    p_scrtext_l = 'Data Lançamento'.
    p_outputlen = 20.
  ENDIF.

  IF p_ref_tabname = 'ZPPT0036_OUT' AND
     p_field       = 'HORA_LANC'.
    p_scrtext_l = 'Hora Lançamento'.
    p_outputlen = 20.
  ENDIF.

ENDFORM.

FORM f_exit_zppt0036_0009  TABLES it_excl_toolbar
                            USING p_db_tab.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
