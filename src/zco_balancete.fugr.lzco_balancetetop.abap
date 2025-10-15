FUNCTION-POOL zco_balancete.                "MESSAGE-ID ..

* INCLUDE LZCO_BALANCETED...                 " Local class definition

"Variáveis de tela
DATA: diretorio     TYPE filename,
      moeda_interna TYPE xflag,
      moeda_forte   TYPE xflag,
      tp_moeda      TYPE xflag,
      moeda_indice  TYPE xflag.
"Tipos
TYPES : BEGIN OF ty_struct,
          col_name(50),
        END OF ty_struct,

        ty_columns TYPE STANDARD TABLE OF ty_struct WITH EMPTY KEY.
"Tabelas internas
DATA : gt_columns              TYPE ty_columns,
       gt_temp                 TYPE dfies_tab,
       gt_saldo_contas         TYPE zsco_saldo_contas_tt,
       gt_saldo_contas_interna TYPE STANDARD TABLE OF zsco_saldo_contas_interna,
       gt_saldo_contas_forte   TYPE STANDARD TABLE OF zsco_saldo_contas_forte,
       gt_tp_moeda             TYPE STANDARD TABLE OF zsco_tp_moeda,
       gt_vlr_moeda            TYPE STANDARD TABLE OF zsco_vlr_moeda,
       ws_tp_moeda             TYPE zsco_tp_moeda,
       gt_saldo_contas_indice  TYPE STANDARD TABLE OF zsco_saldo_contas_indice.

DATA: gs_file_name     TYPE string,
      gv_automoeda     TYPE c,
      gv_moeda_interna TYPE char35,
      gv_moeda_forte   TYPE char35,
      gv_moeda_indice  TYPE char35,
      gv_vlr_brl       TYPE char35,
      gv_vlr_usd       TYPE char35.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_COMPETENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_COMPETENCIA  text
*      <--P_RETURN_STATUS  text
*      <--P_V_DATA_INICIO_APROPRIACAO2  text
*      <--P_V_DATA_FIM_APROPRIACAO2  text
*----------------------------------------------------------------------*
FORM valida_competencia  USING    p_competencia
                         CHANGING p_return_status
                                  p_dt_ini
                                  p_dt_fim.



  DATA: vl_mes         TYPE i,
        vl_ano         TYPE i,
        vl_dt_ini(8)   TYPE c,
        vl_dt_fim(8)   TYPE c,
        vl_dt_low      TYPE sy-datum,
        vl_dt_high_in  TYPE sy-datum,
        vl_dt_high_out TYPE sy-datum.

  vl_mes = p_competencia(02).
  vl_ano = p_competencia+2(04).

  IF ( vl_mes = 0 ) OR ( vl_mes > 12 ).
    p_return_status = 'X'.
    MESSAGE s836(sd) WITH text-e29 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF ( vl_ano = 0 ).
    p_return_status = 'X'.
    MESSAGE s836(sd) WITH text-e30 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CONCATENATE p_competencia+2(4) p_competencia(2) '01' INTO vl_dt_ini.

  CONCATENATE p_competencia+2(4) p_competencia(2) '01' INTO vl_dt_fim.

  vl_dt_low     = vl_dt_ini.
  vl_dt_high_in = vl_dt_fim.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vl_dt_high_in
    IMPORTING
      last_day_of_month = vl_dt_high_out.

  p_dt_ini  = vl_dt_low.
  p_dt_fim  = vl_dt_high_out.

ENDFORM.
