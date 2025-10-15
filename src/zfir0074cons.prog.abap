*&---------------------------------------------------------------------*
*&  Include           ZFIR0074CONS
*&---------------------------------------------------------------------*

*=======================================================================
* Types
*=======================================================================

TYPES: BEGIN OF t_dates,
         date TYPE datum,
         hour TYPE uzeit.
TYPES: END OF t_dates.

TYPES: BEGIN OF t_bukrs,
         bukrs     TYPE zglt037-bukrs,
         bukrs_ate TYPE zglt037-bukrs_ate.
TYPES: END OF t_bukrs.

TYPES: BEGIN OF t_bukrs_check,
         bukrs     TYPE zglt037-bukrs,
         bukrs_ate TYPE zglt037-bukrs_ate,
         vkbur     TYPE zsdt0385-vkbur,
         vkbur_ate TYPE zsdt0385-vkbur_ate.
TYPES: END OF t_bukrs_check.

*=======================================================================
* Variáveis
*=======================================================================

DATA: it_saida_zimp_final      TYPE STANDARD TABLE OF t_zimp_aprovador,            "Tabela para a consistência
      it_key_zimp              TYPE STANDARD TABLE OF t_zimp_aprovador,            "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zimp             TYPE STANDARD TABLE OF t_zimp_aprovador,            "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zimp             TYPE STANDARD TABLE OF t_zimp_aprovador,            "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zimp_final      TYPE t_zimp_aprovador,
      wa_key_zimp              TYPE t_zimp_aprovador,
      wa_temp_zimp             TYPE t_zimp_aprovador,
      wa_loop_zimp             TYPE t_zimp_aprovador,

      it_saida_zglt_final      TYPE STANDARD TABLE OF t_zglt037,                   "Tabela para a consistência
      it_key_zglt              TYPE STANDARD TABLE OF t_zglt037,                   "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zglt             TYPE STANDARD TABLE OF t_zglt037,                   "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zglt             TYPE STANDARD TABLE OF t_zglt037,                   "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zglt_final      TYPE t_zglt037,
      wa_key_zglt              TYPE t_zglt037,
      wa_temp_zglt             TYPE t_zglt037,
      wa_loop_zglt             TYPE t_zglt037,

      it_saida_zov_final       TYPE STANDARD TABLE OF t_zsdt0141,                   "Tabela para a consistência
      it_key_zov               TYPE STANDARD TABLE OF t_zsdt0141,                   "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zov              TYPE STANDARD TABLE OF t_zsdt0141,                   "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zov              TYPE STANDARD TABLE OF t_zsdt0141,                   "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zov_final       TYPE t_zsdt0141,
      wa_key_zov               TYPE t_zsdt0141,
      wa_temp_zov              TYPE t_zsdt0141,
      wa_loop_zov              TYPE t_zsdt0141,

      it_saida_lim_final       TYPE STANDARD TABLE OF t_zsdt0152,                   "Tabela para a consistência
      it_key_lim               TYPE STANDARD TABLE OF t_zsdt0152,                   "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_lim              TYPE STANDARD TABLE OF t_zsdt0152,                   "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_lim              TYPE STANDARD TABLE OF t_zsdt0152,                   "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_lim_final       TYPE t_zsdt0152,
      wa_key_lim               TYPE t_zsdt0152,
      wa_temp_lim              TYPE t_zsdt0152,
      wa_loop_lim              TYPE t_zsdt0152,

      it_saida_zadto_final     TYPE STANDARD TABLE OF t_zadto_aprovador,           "Tabela para a consistência
      it_key_zadto             TYPE STANDARD TABLE OF t_zadto_aprovador,           "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zadto            TYPE STANDARD TABLE OF t_zadto_aprovador,           "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zadto            TYPE STANDARD TABLE OF t_zadto_aprovador,           "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zadto_final     TYPE t_zadto_aprovador,
      wa_key_zadto             TYPE t_zadto_aprovador,
      wa_temp_zadto            TYPE t_zadto_aprovador,
      wa_loop_zadto            TYPE t_zadto_aprovador,

      it_saida_zinv_final      TYPE STANDARD TABLE OF t_zinv_aprovador,            "Tabela para a consistência
      it_key_zinv              TYPE STANDARD TABLE OF t_zinv_aprovador,            "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zinv             TYPE STANDARD TABLE OF t_zinv_aprovador,            "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zinv             TYPE STANDARD TABLE OF t_zinv_aprovador,            "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zinv_final      TYPE t_zinv_aprovador,
      wa_key_zinv              TYPE t_zinv_aprovador,
      wa_temp_zinv             TYPE t_zinv_aprovador,
      wa_loop_zinv             TYPE t_zinv_aprovador,

      it_saida_zfre_final      TYPE STANDARD TABLE OF t_zlest0156,            "Tabela para a consistência
      it_key_zfre              TYPE STANDARD TABLE OF t_zlest0156,            "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zfre             TYPE STANDARD TABLE OF t_zlest0156,            "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zfre             TYPE STANDARD TABLE OF t_zlest0156,            "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zfre_final      TYPE t_zlest0156,
      wa_key_zfre              TYPE t_zlest0156,
      wa_temp_zfre             TYPE t_zlest0156,
      wa_loop_zfre             TYPE t_zlest0156,

      it_saida_zsolov_final    TYPE STANDARD TABLE OF t_zsdt0161,            "Tabela para a consistência
      it_key_zsolov            TYPE STANDARD TABLE OF t_zsdt0161,            "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_zsolov           TYPE STANDARD TABLE OF t_zsdt0161,            "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_zsolov           TYPE STANDARD TABLE OF t_zsdt0161,            "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_zsolov_final    TYPE t_zsdt0161,
      wa_key_zsolov            TYPE t_zsdt0161,
      wa_temp_zsolov           TYPE t_zsdt0161,
      wa_loop_zsolov           TYPE t_zsdt0161,

      it_saida_varcamb_final   TYPE STANDARD TABLE OF t_zmmt0150,            "Tabela para a consistência
      it_key_varcamb           TYPE STANDARD TABLE OF t_zmmt0150,            "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_varcamb          TYPE STANDARD TABLE OF t_zmmt0150,            "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_varcamb          TYPE STANDARD TABLE OF t_zmmt0150,            "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_varcamb_final   TYPE t_zmmt0150,
      wa_key_varcamb           TYPE t_zmmt0150,
      wa_temp_varcamb          TYPE t_zmmt0150,
      wa_loop_varcamb          TYPE t_zmmt0150,

      it_saida_isencao_final   TYPE STANDARD TABLE OF t_zsdt0336,            "Tabela para a consistência
      it_key_isencao           TYPE STANDARD TABLE OF t_zsdt0336,
      "Tabela para as chaves de estratégias (chaves - são únicas)
      it_temp_isencao          TYPE STANDARD TABLE OF t_zsdt0336,            "Tabela para seleção de lançamentos de acordo com a chave
      it_loop_isencao          TYPE STANDARD TABLE OF t_zsdt0336,            "Tabela para seleção de lançamentos com data específica (check de data)
      wa_saida_isencao_final   TYPE t_zsdt0336,
      wa_key_isencao           TYPE t_zsdt0336,
      wa_temp_isencao          TYPE t_zsdt0336,
      wa_loop_isencao          TYPE t_zsdt0336,
      "150184 CS2024000781 Aprovações ZNFW - PSA
      it_key_operznfw          TYPE STANDARD TABLE OF t_zfiwrt0033,
      it_temp_operznfw         TYPE STANDARD TABLE OF t_zfiwrt0033,
      it_loop_operznfw         TYPE STANDARD TABLE OF t_zfiwrt0033,
      wa_saida_operznfw_final  TYPE t_zfiwrt0033,
      wa_key_operznfw          TYPE t_zfiwrt0033,
      wa_temp_operznfw         TYPE t_zfiwrt0033,
      wa_loop_operznfw         TYPE t_zfiwrt0033,

      " 06.05.2025 - 174338 - RAMON -->
      it_key_checklist         TYPE STANDARD TABLE OF t_zsdt0385,
      it_temp_checklist        TYPE STANDARD TABLE OF t_zsdt0385,
      it_loop_checklist        TYPE STANDARD TABLE OF t_zsdt0385,
      wa_saida_checklist_final TYPE t_zsdt0385,
      wa_key_checklist         TYPE t_zsdt0385,
      wa_temp_checklist        TYPE t_zsdt0385,
      wa_loop_checklist        TYPE t_zsdt0385.
" 06.05.2025 - 174338 - RAMON --<


DATA: it_dates TYPE STANDARD TABLE OF t_dates,                               "Tabela para registro de datas de estratégia
      wa_dates TYPE t_dates.

DATA: it_bukrs TYPE STANDARD TABLE OF t_bukrs,                               "Tabela para registro de datas de estratégia
      wa_bukrs TYPE t_bukrs.

DATA: it_bukrs_c TYPE STANDARD TABLE OF t_bukrs_check,                               "Tabela para registro de datas de estratégia
      wa_bukrs_c TYPE t_bukrs_check.

DATA: nivel       TYPE p,                                                       "Variável para o check de nível
      nivel_check TYPE p,                                                       "Variável para o check de nível
      c_nivel_min TYPE p,
      c_nivel_max TYPE p,
      nivel_max   TYPE p,
      valor_de    TYPE p DECIMALS 2,                                            "Variável para o check de faixa de valor de aprovação
      valor_ate   TYPE p DECIMALS 2,                                            "Variável para o check de faixa de valor de aprovação
      range_min   TYPE p DECIMALS 2,
      range_max   TYPE p DECIMALS 2,
      c_range     TYPE p.

DATA: linha_01 TYPE i,                                                          "Variável para consistência de estratégias duplicadas ou aninhadas
      linha_02 TYPE i.                                                          "Variável para consistência de estratégias duplicadas ou aninhadas


FORM consistencia_zimp .

  LOOP AT it_saida_zimp INTO wa_saida_zimp.
    "/Consistência na Estratégia para falta de dados./
    IF wa_saida_zimp-bukrs        IS INITIAL
      OR wa_saida_zimp-bukrs_ate  IS INITIAL
      OR wa_saida_zimp-dep_resp   IS INITIAL
      OR wa_saida_zimp-aprovador  IS INITIAL
      OR wa_saida_zimp-waers      IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zimp.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_zimp-motivo IS INITIAL AND wa_saida_zimp-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zimp.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_zimp-ck_ant NE 'X' AND wa_saida_zimp-dt_val_de < sy-datum )
      OR  ( wa_saida_zimp-ck_ant NE 'X' AND wa_saida_zimp-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zimp.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zimp-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zimp.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zimp-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zimp.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.


  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab1.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zimp = it_saida_zimp_final.

  SORT it_key_zimp BY bukrs       ASCENDING
                      bukrs_ate   ASCENDING
                      dep_resp    ASCENDING
                      waers       ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_zimp COMPARING bukrs bukrs_ate dep_resp.

  LOOP AT it_key_zimp INTO wa_key_zimp.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_zimp = it_saida_zimp_final.
    DELETE it_temp_zimp
      WHERE bukrs     NE wa_key_zimp-bukrs
        OR bukrs_ate  NE wa_key_zimp-bukrs_ate
        OR dep_resp   NE wa_key_zimp-dep_resp
        OR waers      NE wa_key_zimp-waers.

    CLEAR: it_bukrs.
    REFRESH: it_bukrs.

    LOOP AT it_temp_zimp INTO wa_temp_zimp.
      wa_bukrs-bukrs = wa_temp_zimp-bukrs.
      wa_bukrs-bukrs_ate = wa_temp_zimp-bukrs_ate.
      APPEND wa_bukrs TO it_bukrs.
    ENDLOOP.

    SORT it_bukrs BY bukrs bukrs_ate ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_bukrs COMPARING ALL FIELDS.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_bukrs INTO wa_bukrs.
      it_loop_zimp = it_temp_zimp.

      DELETE it_loop_zimp WHERE bukrs      NE wa_bukrs-bukrs
                          AND   bukrs_ate  NE wa_bukrs-bukrs_ate.

      wa_dates-hour  = sy-uzeit + 1.
      DELETE it_loop_zimp
        WHERE dt_val_ate LE sy-datum
        AND   hr_val_ate LT wa_dates-hour.
*        AND   mandt      EQ ''. "ALRS

      DELETE it_loop_zimp
       WHERE ( dt_val_ate LT sy-datum OR  dt_val_de GT sy-datum )
       AND   mandt      NE ''.

      DELETE it_loop_zimp
       WHERE dt_val_de  GT sy-datum
       AND   transf_aprov EQ 'P'.

      SORT it_loop_zimp BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zimp INTO wa_loop_zimp.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zimp-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zimp-nivel IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Nível Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE |Estratégia fora de sequência Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zimp-valor_de IS INITIAL OR wa_loop_zimp-valor_ate IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Faixa de Valor Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zimp-valor_de GE wa_loop_zimp-valor_ate.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia com Faixa de Valor incorreta Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zimp-valor_de GT wa_loop_zimp-valor_ate.
*              MESSAGE 'Estratégia com faixa de valores errada! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores errada Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_zimp-valor_de.
              valor_ate = wa_loop_zimp-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zimp-valor_de = valor_ate + '0.01' AND wa_loop_zimp-valor_ate GT wa_loop_zimp-valor_de.
              valor_ate = wa_loop_zimp-valor_ate.
            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores fora de sequência Depto { wa_loop_zimp-dep_resp } usuário { wa_loop_zimp-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zimp, valor_de, valor_ate.
      it_loop_zimp = it_temp_zimp.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zimp.
  ENDLOOP.
*  LOOP AT IT_KEY_ZIMP INTO WA_KEY_ZIMP.
*
*    "obtendo tabela temporária de acordo com a chave de estratégia
*    IT_TEMP_ZIMP = IT_SAIDA_ZIMP_FINAL.
*    DELETE IT_TEMP_ZIMP
*      WHERE BUKRS     NE WA_KEY_ZIMP-BUKRS
*        OR BUKRS_ATE  NE WA_KEY_ZIMP-BUKRS_ATE
*        OR DEP_RESP   NE WA_KEY_ZIMP-DEP_RESP
*        OR WAERS      NE WA_KEY_ZIMP-WAERS.
*
*    "populando a tabela de datas a serem investigadas
*    CLEAR: IT_DATES.
*
*    LOOP AT IT_TEMP_ZIMP INTO WA_TEMP_ZIMP.
*      WA_DATES-DATE = WA_TEMP_ZIMP-DT_VAL_DE.
*      WA_DATES-HOUR = WA_TEMP_ZIMP-HR_VAL_DE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZIMP-HR_VAL_DE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZIMP-DT_VAL_DE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZIMP-DT_VAL_DE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*      WA_DATES-DATE = WA_TEMP_ZIMP-DT_VAL_ATE.
*      WA_DATES-HOUR = WA_TEMP_ZIMP-HR_VAL_ATE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZIMP-HR_VAL_ATE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZIMP-DT_VAL_ATE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZIMP-DT_VAL_ATE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*    ENDLOOP.
*
*    SORT IT_DATES BY DATE ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM IT_DATES COMPARING DATE HOUR.
*
*    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
*    LOOP AT IT_DATES INTO WA_DATES.
*
*      IT_LOOP_ZIMP = IT_TEMP_ZIMP.
*
*      "IF WA_DATES IS NOT INITIAL.
*      DELETE IT_LOOP_ZIMP
*        WHERE ( DT_VAL_DE < WA_DATES-DATE AND DT_VAL_ATE < WA_DATES-DATE )
*          OR ( DT_VAL_DE > WA_DATES-DATE AND DT_VAL_ATE > WA_DATES-DATE ).
*      "ENDIF.
*
*      DELETE IT_LOOP_ZIMP
*        WHERE DT_VAL_ATE < WA_DATES-DATE "WA_DATES-DATE > DT_VAL_ATE
*        OR  DT_VAL_DE > WA_DATES-DATE "WA_DATES-DATE < DT_VAL_DE
*        OR ( DT_VAL_DE EQ WA_DATES-DATE AND HR_VAL_DE > WA_DATES-HOUR )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
*        OR ( DT_VAL_ATE EQ WA_DATES-DATE AND HR_VAL_ATE < WA_DATES-HOUR ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).
*
*      SORT IT_LOOP_ZIMP BY NIVEL ASCENDING.
*
*      "loop na tabela loop para consistência de nível e faixa de valor
*      LOOP AT IT_LOOP_ZIMP INTO WA_LOOP_ZIMP.
*        "consistência de nível
*        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
*          EXPORTING
*            CHR = WA_LOOP_ZIMP-NIVEL
*          IMPORTING
*            NUM = NIVEL.
*
*        IF WA_LOOP_ZIMP-NIVEL IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
*          LEAVE TO SCREEN 9000.
*        ELSE.
*          IF SY-TABIX EQ 1.
*            IF NIVEL NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: NIVEL, NIVEL_CHECK.
*              LEAVE TO SCREEN 9000.
*            ELSE.
*              NIVEL_CHECK = NIVEL.
*            ENDIF.
*          ELSE.
*            IF NIVEL = NIVEL_CHECK + 1.
*              NIVEL_CHECK = NIVEL.
*            ELSE.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: NIVEL, NIVEL_CHECK.
*              LEAVE TO SCREEN 9000.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*        CLEAR: NIVEL.
*        "consistência de faixa de valor
*        IF WA_LOOP_ZIMP-VALOR_DE IS INITIAL OR WA_LOOP_ZIMP-VALOR_ATE IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
*          LEAVE TO SCREEN 9000.
*        ELSEIF WA_LOOP_ZIMP-VALOR_DE GE WA_LOOP_ZIMP-VALOR_ATE.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
*          CLEAR: VALOR_DE, VALOR_ATE.
*          LEAVE TO SCREEN 9000.
*        ELSE.
*          IF SY-TABIX EQ 1.
*            IF WA_LOOP_ZIMP-VALOR_DE GT WA_LOOP_ZIMP-VALOR_ATE.
*              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: VALOR_DE, VALOR_ATE.
*              LEAVE TO SCREEN 9000.
*            ELSE.
*              VALOR_DE = WA_LOOP_ZIMP-VALOR_DE.
*              VALOR_ATE = WA_LOOP_ZIMP-VALOR_ATE.
*            ENDIF.
*          ELSE.
*            IF WA_LOOP_ZIMP-VALOR_DE = VALOR_ATE + '0.01' AND WA_LOOP_ZIMP-VALOR_ATE GT WA_LOOP_ZIMP-VALOR_DE.
*              VALOR_ATE = WA_LOOP_ZIMP-VALOR_ATE.
*            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: VALOR_DE, VALOR_ATE.
*              LEAVE TO SCREEN 9000.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      CLEAR: NIVEL_CHECK, WA_LOOP_ZIMP, VALOR_DE, VALOR_ATE.
*      IT_LOOP_ZIMP = IT_TEMP_ZIMP.
*    ENDLOOP.
*    CLEAR: IT_DATES, IT_LOOP_ZIMP.
*  ENDLOOP.
  CLEAR: it_temp_zimp.
  "consistência de estratégias duplicadas ou aninhadas.
  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.

  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zimp INTO wa_key_zimp.

    it_temp_zimp = it_saida_zimp_final.

    DELETE it_temp_zimp
          WHERE bukrs     NE wa_key_zimp-bukrs
            OR bukrs_ate  NE wa_key_zimp-bukrs_ate
            OR dep_resp   NE wa_key_zimp-dep_resp
            OR waers      NE wa_key_zimp-waers
            OR dt_val_ate LT sy-datum
            OR ( dt_val_ate EQ sy-datum AND
                 hr_val_ate LT sy-uzeit ).

    "ALRS
    LOOP AT it_temp_zimp INTO wa_temp_zimp.
      IF wa_temp_zimp-dt_val_de = wa_temp_zimp-dt_val_ate AND
         wa_temp_zimp-hr_val_de = wa_temp_zimp-hr_val_ate.
        wa_temp_zimp-ck_ant = 'D'.
        MODIFY it_temp_zimp FROM wa_temp_zimp INDEX sy-tabix TRANSPORTING ck_ant.
      ENDIF.
    ENDLOOP.
    DELETE it_temp_zimp WHERE ck_ant = 'D'.

    DESCRIBE TABLE it_temp_zimp LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zimp.

      it_temp_zimp = it_saida_zimp_final.

      DELETE it_temp_zimp
        WHERE bukrs       < wa_key_zimp-bukrs
          OR bukrs_ate    > wa_key_zimp-bukrs_ate
          OR dep_resp     NE wa_key_zimp-dep_resp
          OR waers        NE wa_key_zimp-waers
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      "ALRS
      LOOP AT it_temp_zimp INTO wa_temp_zimp.
        IF wa_temp_zimp-dt_val_de = wa_temp_zimp-dt_val_ate AND
           wa_temp_zimp-hr_val_de = wa_temp_zimp-hr_val_ate.
          wa_temp_zimp-ck_ant = 'D'.
          MODIFY it_temp_zimp FROM wa_temp_zimp INDEX sy-tabix TRANSPORTING ck_ant.
        ENDIF.
      ENDLOOP.
      DELETE it_temp_zimp WHERE ck_ant = 'D'.

      SORT it_temp_zimp BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zimp COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zimp LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zimp.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_zglt .

  LOOP AT it_saida_zglt INTO wa_saida_zglt.
    "/Consistência na Estratégia para falta de dados./
    IF wa_saida_zglt-bukrs        IS INITIAL
      OR wa_saida_zglt-bukrs_ate  IS INITIAL
      OR wa_saida_zglt-dep_resp   IS INITIAL
      OR wa_saida_zglt-aprovador  IS INITIAL
      OR wa_saida_zglt-waers      IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zglt.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "Verificar se o motivo esta preenchido.
    IF wa_saida_zglt-motivo IS INITIAL AND wa_saida_zglt-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zglt.
      LEAVE TO SCREEN 9000.
    ENDIF.


    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_zglt-ck_ant NE 'X' AND wa_saida_zglt-dt_val_de < sy-datum )
      OR  ( wa_saida_zglt-ck_ant NE 'X' AND wa_saida_zglt-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zglt.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zglt-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zglt.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zglt-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zglt.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

*  IF v_salvar EQ '8'.
*    i_main_tab-pressed_tab = c_main_tab-tab2.
*  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zglt = it_saida_zglt_final.

  SORT it_key_zglt BY bukrs       ASCENDING
                      bukrs_ate   ASCENDING
                      dep_resp    ASCENDING
                      waers       ASCENDING
                      pgt_forn    ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_zglt COMPARING bukrs bukrs_ate dep_resp waers pgt_forn.

  LOOP AT it_key_zglt INTO wa_key_zglt.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_zglt = it_saida_zglt_final.
    DELETE it_temp_zglt
      WHERE bukrs     NE wa_key_zglt-bukrs
        OR bukrs_ate  NE wa_key_zglt-bukrs_ate
        OR dep_resp   NE wa_key_zglt-dep_resp
        OR waers      NE wa_key_zglt-waers
        OR pgt_forn NE wa_key_zglt-pgt_forn.
    CLEAR: it_bukrs.
    REFRESH: it_bukrs.

    LOOP AT it_temp_zglt INTO wa_temp_zglt.
      wa_bukrs-bukrs = wa_temp_zglt-bukrs.
      wa_bukrs-bukrs_ate = wa_temp_zglt-bukrs_ate.
      APPEND wa_bukrs TO it_bukrs.
    ENDLOOP.

    SORT it_bukrs BY bukrs bukrs_ate ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_bukrs COMPARING ALL FIELDS.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_bukrs INTO wa_bukrs.
      it_loop_zglt = it_temp_zglt.

      DELETE it_loop_zglt WHERE bukrs      NE wa_bukrs-bukrs
                          AND   bukrs_ate  NE wa_bukrs-bukrs_ate.

      wa_dates-hour  = sy-uzeit + 1.
      DELETE it_loop_zglt
        WHERE dt_val_ate LE sy-datum
        AND   hr_val_ate LT wa_dates-hour.
      " AND   mandt      EQ ''. ALRS

      DELETE it_loop_zglt
       WHERE ( dt_val_ate LT sy-datum OR  dt_val_de GT sy-datum )
       AND   mandt      NE ''.

      DELETE it_loop_zglt
       WHERE dt_val_de  GT sy-datum
       AND   transf_aprov EQ 'P'.

      SORT it_loop_zglt BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zglt INTO wa_loop_zglt.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zglt-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zglt-nivel IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Nível Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE |Estratégia fora de sequência Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zglt-valor_de IS INITIAL OR wa_loop_zglt-valor_ate IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Faixa de Valor Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zglt-valor_de GE wa_loop_zglt-valor_ate.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia com Faixa de Valor incorreta Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zglt-valor_de GT wa_loop_zglt-valor_ate.
*              MESSAGE 'Estratégia com faixa de valores errada! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores errada Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_zglt-valor_de.
              valor_ate = wa_loop_zglt-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zglt-valor_de = valor_ate + '0.01' AND wa_loop_zglt-valor_ate GT wa_loop_zglt-valor_de.
              valor_ate = wa_loop_zglt-valor_ate.
            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores fora de sequência Depto { wa_loop_zglt-dep_resp } usuário { wa_loop_zglt-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zglt, valor_de, valor_ate.
      it_loop_zglt = it_temp_zglt.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zglt.
  ENDLOOP.
  CLEAR: it_temp_zglt.

  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.

  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zglt INTO wa_key_zglt.

    it_temp_zglt = it_saida_zglt_final.

    DELETE it_temp_zglt
          WHERE bukrs     NE wa_key_zglt-bukrs
            OR bukrs_ate  NE wa_key_zglt-bukrs_ate
            OR dep_resp   NE wa_key_zglt-dep_resp
            OR waers      NE wa_key_zglt-waers
            OR pgt_forn   NE wa_key_zglt-pgt_forn
            OR dt_val_ate LT sy-datum
            OR ( dt_val_ate EQ sy-datum AND
                 hr_val_ate LT sy-uzeit ).

    "ALRS
    LOOP AT it_temp_zglt INTO wa_temp_zglt.
      IF wa_temp_zglt-dt_val_de = wa_temp_zglt-dt_val_ate AND
         wa_temp_zglt-hr_val_de = wa_temp_zglt-hr_val_ate.
        wa_temp_zglt-ck_ant = 'D'.
        MODIFY it_temp_zglt FROM wa_temp_zglt INDEX sy-tabix TRANSPORTING ck_ant.
      ENDIF.
    ENDLOOP.
    DELETE it_temp_zglt WHERE ck_ant = 'D'.

    DESCRIBE TABLE it_temp_zglt LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zglt.

      it_temp_zglt = it_saida_zglt_final.

      DELETE it_temp_zglt
        WHERE bukrs     < wa_key_zglt-bukrs
          OR bukrs_ate  > wa_key_zglt-bukrs_ate
          OR dep_resp   NE wa_key_zglt-dep_resp
          OR waers      NE wa_key_zglt-waers
          OR pgt_forn   NE wa_key_zglt-pgt_forn
          OR dt_val_ate LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      "ALRS
      LOOP AT it_temp_zglt INTO wa_temp_zglt.
        IF wa_temp_zglt-dt_val_de = wa_temp_zglt-dt_val_ate AND
           wa_temp_zglt-hr_val_de = wa_temp_zglt-hr_val_ate.
          wa_temp_zglt-ck_ant = 'D'.
          MODIFY it_temp_zglt FROM wa_temp_zglt INDEX sy-tabix TRANSPORTING ck_ant.
        ENDIF.
      ENDLOOP.
      DELETE it_temp_zglt WHERE ck_ant = 'D'.


      SORT it_temp_zglt BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zglt COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zglt LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zglt.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_zov .

  LOOP AT it_saida_zov INTO wa_saida_zov.
    "/Consistência na Estratégia para falta de dados./
    IF wa_saida_zov-bukrs        IS INITIAL
      OR wa_saida_zov-bukrs_ate  IS INITIAL
      OR wa_saida_zov-vkbur      IS INITIAL
      OR wa_saida_zov-vkbur_ate  IS INITIAL
      OR wa_saida_zov-aprovador  IS INITIAL
      OR wa_saida_zov-waers      IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zov.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência se o motivo esta preenchido.
    IF wa_saida_zov-motivo  IS INITIAL AND wa_saida_zov-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zov.
      LEAVE TO SCREEN 9000.
    ENDIF.

    " 09.05.2025 - RAMON -->
    IF wa_saida_zov-waers <> 'USD'.
      MESSAGE 'A moeda só pode ser Dólar(USD)' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zov.
      LEAVE TO SCREEN 9000.
    ENDIF.
    " 09.05.2025 - RAMON --<

    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_zov-ck_ant NE 'X' AND wa_saida_zov-dt_val_de < sy-datum )
      OR  ( wa_saida_zov-ck_ant NE 'X' AND wa_saida_zov-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zov.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zov-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zov.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zov-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zov.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab5.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zov = it_saida_zov_final.

  SORT it_key_zov BY bukrs       ASCENDING
                     bukrs_ate   ASCENDING
                     vkbur       ASCENDING
                     vkbur_ate   ASCENDING
                     waers       ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_zov COMPARING bukrs  bukrs_ate vkbur vkbur_ate waers.

  LOOP AT it_key_zov INTO wa_key_zov.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_zov = it_saida_zov_final.
    DELETE it_temp_zov
      WHERE bukrs      NE wa_key_zov-bukrs
         OR bukrs_ate  NE wa_key_zov-bukrs_ate
         OR vkbur      NE wa_key_zov-vkbur
         OR vkbur_ate  NE wa_key_zov-vkbur_ate
         OR waers      NE wa_key_zov-waers.

    "populando a tabela de datas a serem investigadas
    CLEAR: it_dates.

    LOOP AT it_temp_zov INTO wa_temp_zov.
      wa_dates-date = wa_temp_zov-dt_val_de.
      wa_dates-hour = wa_temp_zov-hr_val_de.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_zov-hr_val_de + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_zov-dt_val_de + 1.
        ELSE.
          wa_dates-date = wa_temp_zov-dt_val_de.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
      wa_dates-date = wa_temp_zov-dt_val_ate.
      wa_dates-hour = wa_temp_zov-hr_val_ate.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_zov-hr_val_ate + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_zov-dt_val_ate + 1.
        ELSE.
          wa_dates-date = wa_temp_zov-dt_val_ate.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
    ENDLOOP.

    SORT it_dates BY date ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_dates COMPARING date hour.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_dates INTO wa_dates.

      it_loop_zov = it_temp_zov.

      "IF WA_DATES IS NOT INITIAL.
      DELETE it_loop_zov
        WHERE ( dt_val_de < wa_dates-date AND dt_val_ate < wa_dates-date )
          OR ( dt_val_de > wa_dates-date AND dt_val_ate > wa_dates-date ).
      "ENDIF.

      DELETE it_loop_zov
      WHERE dt_val_ate < wa_dates-date "WA_DATES-DATE > DT_VAL_ATE
      OR  dt_val_de > wa_dates-date "WA_DATES-DATE < DT_VAL_DE
      OR ( dt_val_de EQ wa_dates-date AND hr_val_de > wa_dates-hour )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
      OR ( dt_val_ate EQ wa_dates-date AND hr_val_ate < wa_dates-hour ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).

      SORT it_loop_zov BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zov INTO wa_loop_zov.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zov-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zov-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zov-valor_de IS INITIAL OR wa_loop_zov-valor_ate IS INITIAL.
          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zov-valor_de GE wa_loop_zov-valor_ate.
          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zov-valor_de GT wa_loop_zov-valor_ate.
              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_zov-valor_de.
              valor_ate = wa_loop_zov-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zov-valor_de = valor_ate + '0.01' AND wa_loop_zov-valor_ate GT wa_loop_zov-valor_de.
              valor_ate = wa_loop_zov-valor_ate.
            ELSE.
              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zov, valor_de, valor_ate.
      it_loop_zov = it_temp_zov.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zov.
  ENDLOOP.
  CLEAR: it_temp_zov.

  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.

  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zov INTO wa_key_zov.

    it_temp_zov = it_saida_zov_final.

    DELETE it_temp_zov
          WHERE bukrs     NE wa_key_zov-bukrs
            OR bukrs_ate  NE wa_key_zov-bukrs_ate
            OR vkbur      NE wa_key_zov-vkbur
            OR vkbur_ate  NE wa_key_zov-vkbur_ate
            OR waers      NE wa_key_zov-waers
            OR dt_val_ate LT sy-datum
            OR ( dt_val_ate EQ sy-datum AND
                 hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_zov LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zov.

      it_temp_zov = it_saida_zov_final.

      DELETE it_temp_zov
        WHERE bukrs      < wa_key_zov-bukrs
          OR  bukrs_ate  > wa_key_zov-bukrs_ate
          OR  vkbur      < wa_key_zov-vkbur
          OR  vkbur_ate  > wa_key_zov-vkbur_ate
          OR  waers      NE wa_key_zov-waers
          OR dt_val_ate LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_zov BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zov COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zov LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zglt.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_lim .

  LOOP AT it_saida_lim INTO wa_saida_lim.
    "/Consistência na Estratégia para falta de dados./
    IF   wa_saida_lim-werks      IS INITIAL
      OR wa_saida_lim-werks_ate  IS INITIAL
      OR wa_saida_lim-vkorg      IS INITIAL
      OR wa_saida_lim-aprovador  IS INITIAL
      OR wa_saida_lim-waers      IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_lim.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência se o motivo esta preenchido.
    IF wa_saida_lim-motivo  IS INITIAL AND wa_saida_lim-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_lim.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_lim-ck_ant NE 'X' AND wa_saida_lim-dt_val_de < sy-datum )
      OR  ( wa_saida_lim-ck_ant NE 'X' AND wa_saida_lim-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_lim.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "Valida Empresa
    SELECT SINGLE *
      FROM t001w
      INTO @DATA(wa_t001_)
      WHERE werks =  @wa_saida_lim-werks.
    IF sy-subrc NE 0.
      MESSAGE 'Centro DE, inválido' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_lim.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001w
     INTO @DATA(wa_t001)
     WHERE werks =  @wa_saida_lim-werks_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Centro ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR: wa_saida_lim.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab6.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_lim = it_saida_lim_final.

  SORT it_key_lim BY vkorg       ASCENDING
                     werks       ASCENDING
                     werks_ate   ASCENDING
                     waers       ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_lim COMPARING vkorg werks werks_ate waers.

  LOOP AT it_key_lim INTO wa_key_lim.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_lim = it_saida_lim_final.
    DELETE it_temp_lim
      WHERE vkorg      NE wa_key_lim-vkorg
         OR werks      NE wa_key_lim-werks
         OR werks_ate  NE wa_key_lim-werks_ate
         OR waers      NE wa_key_lim-waers.

    "populando a tabela de datas a serem investigadas
    CLEAR: it_dates.

    LOOP AT it_temp_lim INTO wa_temp_lim.
      wa_dates-date = wa_temp_lim-dt_val_de.
      wa_dates-hour = wa_temp_lim-hr_val_de.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_lim-hr_val_de + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_lim-dt_val_de + 1.
        ELSE.
          wa_dates-date = wa_temp_lim-dt_val_de.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
      wa_dates-date = wa_temp_lim-dt_val_ate.
      wa_dates-hour = wa_temp_lim-hr_val_ate.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_lim-hr_val_ate + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_lim-dt_val_ate + 1.
        ELSE.
          wa_dates-date = wa_temp_lim-dt_val_ate.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
    ENDLOOP.

    SORT it_dates BY date ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_dates COMPARING date hour.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_dates INTO wa_dates.

      it_loop_lim = it_temp_lim.

      "IF WA_DATES IS NOT INITIAL.
      DELETE it_loop_lim
        WHERE ( dt_val_de < wa_dates-date AND dt_val_ate < wa_dates-date )
          OR ( dt_val_de > wa_dates-date AND dt_val_ate > wa_dates-date ).
      "ENDIF.

      DELETE it_loop_lim
      WHERE dt_val_ate < wa_dates-date "WA_DATES-DATE > DT_VAL_ATE
      OR  dt_val_de > wa_dates-date "WA_DATES-DATE < DT_VAL_DE
      OR ( dt_val_de EQ wa_dates-date AND hr_val_de > wa_dates-hour )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
      OR ( dt_val_ate EQ wa_dates-date AND hr_val_ate < wa_dates-hour ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).

      SORT it_loop_lim BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_lim INTO wa_loop_lim.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_lim-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_lim-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_lim-valor_de IS INITIAL OR wa_loop_lim-valor_ate IS INITIAL.
          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_lim-valor_de GE wa_loop_lim-valor_ate.
          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_lim-valor_de GT wa_loop_lim-valor_ate.
              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_lim-valor_de.
              valor_ate = wa_loop_lim-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_lim-valor_de = valor_ate + '0.01' AND wa_loop_lim-valor_ate GT wa_loop_lim-valor_de.
              valor_ate = wa_loop_lim-valor_ate.
            ELSE.
              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_lim, valor_de, valor_ate.
      it_loop_lim = it_temp_lim.
    ENDLOOP.
    CLEAR: it_dates, it_loop_lim.
  ENDLOOP.
  CLEAR: it_temp_lim.

  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.

  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_lim INTO wa_key_lim.

    it_temp_lim = it_saida_lim_final.

    DELETE it_temp_lim
          WHERE vkorg     NE wa_key_lim-vkorg
            OR werks      NE wa_key_lim-werks
            OR werks_ate  NE wa_key_lim-werks_ate
            OR waers      NE wa_key_lim-waers
            OR dt_val_ate LT sy-datum
            OR ( dt_val_ate EQ sy-datum AND
                 hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_lim LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_lim.

      it_temp_lim = it_saida_lim_final.

      DELETE it_temp_lim
        WHERE vkorg      NE wa_key_lim-vkorg
          OR  werks      < wa_key_lim-werks
          OR  werks_ate  > wa_key_lim-werks_ate
          OR  waers  NE wa_key_lim-waers
          OR dt_val_ate LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_lim BY nivel valor_de valor_ate dt_val_de dt_val_ate waers ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_lim COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate waers.

      DESCRIBE TABLE it_temp_lim LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zglt.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_zadto .

  LOOP AT it_saida_zadto INTO wa_saida_zadto.
    "/Consistência na Estratégia para falta de dados./
    IF wa_saida_zadto-bukrs        IS INITIAL
      OR wa_saida_zadto-bukrs_ate  IS INITIAL
      OR wa_saida_zadto-dep_resp   IS INITIAL
      OR wa_saida_zadto-aprovador  IS INITIAL
      OR wa_saida_zadto-waers      IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zadto.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência na motivo esta preenchido.
    IF wa_saida_zadto-motivo IS INITIAL AND wa_saida_zadto-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zadto.
      LEAVE TO SCREEN 9000.
    ENDIF.


    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_zadto-ck_ant NE 'X' AND wa_saida_zadto-dt_val_de < sy-datum )
      OR  ( wa_saida_zadto-ck_ant NE 'X' AND wa_saida_zadto-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zadto.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zadto-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zadto.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zadto-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR: wa_saida_zadto.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab4.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zadto = it_saida_zadto_final.

  SORT it_key_zadto BY bukrs       ASCENDING
                      bukrs_ate   ASCENDING
                      dep_resp    ASCENDING
                      waers       ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_zadto COMPARING bukrs bukrs_ate dep_resp waers.
  LOOP AT it_key_zadto INTO wa_key_zadto.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_zadto = it_saida_zadto_final.
    DELETE it_temp_zadto
      WHERE bukrs     NE wa_key_zadto-bukrs
        OR bukrs_ate  NE wa_key_zadto-bukrs_ate
        OR dep_resp   NE wa_key_zadto-dep_resp
        OR waers      NE wa_key_zadto-waers.

    CLEAR: it_bukrs.
    REFRESH: it_bukrs.

    LOOP AT it_temp_zadto INTO wa_temp_zadto.
      wa_bukrs-bukrs = wa_temp_zadto-bukrs.
      wa_bukrs-bukrs_ate = wa_temp_zadto-bukrs_ate.
      APPEND wa_bukrs TO it_bukrs.
    ENDLOOP.

    SORT it_bukrs BY bukrs bukrs_ate ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_bukrs COMPARING ALL FIELDS.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_bukrs INTO wa_bukrs.
      it_loop_zadto = it_temp_zadto.

      DELETE it_loop_zadto WHERE bukrs      NE wa_bukrs-bukrs
                          AND   bukrs_ate  NE wa_bukrs-bukrs_ate.

      wa_dates-hour  = sy-uzeit + 1.
      DELETE it_loop_zadto
        WHERE dt_val_ate LE sy-datum
        AND   hr_val_ate LT wa_dates-hour.
*        AND   mandt      EQ ''. "ALRS

      DELETE it_loop_zadto
       WHERE ( dt_val_ate LT sy-datum OR  dt_val_de GT sy-datum )
       AND   mandt      NE ''.

      DELETE it_loop_zadto
       WHERE dt_val_de  GT sy-datum
       AND   transf_aprov EQ 'P'.

      SORT it_loop_zadto BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zadto INTO wa_loop_zadto.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zadto-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zadto-nivel IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Nível Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE |Estratégia fora de sequência Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zadto-valor_de IS INITIAL OR wa_loop_zadto-valor_ate IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Faixa de Valor Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zadto-valor_de GE wa_loop_zadto-valor_ate.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia com Faixa de Valor incorreta Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zadto-valor_de GT wa_loop_zadto-valor_ate.
*              MESSAGE 'Estratégia com faixa de valores errada! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores errada Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_zadto-valor_de.
              valor_ate = wa_loop_zadto-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zadto-valor_de = valor_ate + '0.01' AND wa_loop_zadto-valor_ate GT wa_loop_zadto-valor_de.
              valor_ate = wa_loop_zadto-valor_ate.
            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores fora de sequência Depto { wa_loop_zadto-dep_resp } usuário { wa_loop_zadto-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zadto, valor_de, valor_ate.
      it_loop_zadto = it_temp_zadto.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zadto.
  ENDLOOP.

*  LOOP AT IT_KEY_ZADTO INTO WA_KEY_ZADTO.
*
*    "obtendo tabela temporária de acrodo com a chave de estratégia
*    IT_TEMP_ZADTO = IT_SAIDA_ZADTO_FINAL.
*    DELETE IT_TEMP_ZADTO
*      WHERE BUKRS     NE WA_KEY_ZADTO-BUKRS
*        OR BUKRS_ATE  NE WA_KEY_ZADTO-BUKRS_ATE
*        OR DEP_RESP   NE WA_KEY_ZADTO-DEP_RESP
*        OR WAERS      NE WA_KEY_ZADTO-WAERS.
*
*    "populando a tabela de datas a serem investigadas
*    CLEAR: IT_DATES.

*    LOOP AT IT_TEMP_ZADTO INTO WA_TEMP_ZADTO.
*      WA_DATES-DATE = WA_TEMP_ZADTO-DT_VAL_DE.
*      WA_DATES-HOUR = WA_TEMP_ZADTO-HR_VAL_DE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZADTO-HR_VAL_DE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZADTO-DT_VAL_DE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZADTO-DT_VAL_DE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*      WA_DATES-DATE = WA_TEMP_ZADTO-DT_VAL_ATE.
*      WA_DATES-HOUR = WA_TEMP_ZADTO-HR_VAL_ATE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZADTO-HR_VAL_ATE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZADTO-DT_VAL_ATE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZADTO-DT_VAL_ATE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*    ENDLOOP.
*
*    SORT IT_DATES BY DATE ASCENDING.
*
*    DELETE ADJACENT DUPLICATES FROM IT_DATES COMPARING DATE HOUR.
*
*    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
*    LOOP AT IT_DATES INTO WA_DATES.
*
*      IT_LOOP_ZADTO = IT_TEMP_ZADTO.
*
*      "IF WA_DATES IS NOT INITIAL.
*      DELETE IT_LOOP_ZADTO
*        WHERE ( DT_VAL_DE < WA_DATES-DATE AND DT_VAL_ATE < WA_DATES-DATE )
*          OR ( DT_VAL_DE > WA_DATES-DATE AND DT_VAL_ATE > WA_DATES-DATE ).
*      "ENDIF.
*
*      DELETE IT_LOOP_ZADTO
*            WHERE DT_VAL_ATE < WA_DATES-DATE "WA_DATES-DATE > DT_VAL_ATE
*            OR  DT_VAL_DE > WA_DATES-DATE "WA_DATES-DATE < DT_VAL_DE
*            OR ( DT_VAL_DE EQ WA_DATES-DATE AND HR_VAL_DE > WA_DATES-HOUR )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
*            OR ( DT_VAL_ATE EQ WA_DATES-DATE AND HR_VAL_ATE < WA_DATES-HOUR ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).
*
*
*      SORT IT_LOOP_ZADTO BY NIVEL ASCENDING.
*
*      "loop na tabela loop para consistência de nível e faixa de valor
*      LOOP AT IT_LOOP_ZADTO INTO WA_LOOP_ZADTO.
*        "consistência de nível
*        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
*          EXPORTING
*            CHR = WA_LOOP_ZADTO-NIVEL
*          IMPORTING
*            NUM = NIVEL.
*
*        IF WA_LOOP_ZADTO-NIVEL IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
*          LEAVE TO SCREEN 9000.
*        ELSE.
*          IF SY-TABIX EQ 1.
*            IF NIVEL NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: NIVEL, NIVEL_CHECK.
*              LEAVE TO SCREEN 9000.
*            ELSE.
*              NIVEL_CHECK = NIVEL.
*            ENDIF.
*          ELSE.
*            IF NIVEL = NIVEL_CHECK + 1.
*              NIVEL_CHECK = NIVEL.
*            ELSE.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: NIVEL, NIVEL_CHECK.
*              LEAVE TO SCREEN 9000.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*        CLEAR: NIVEL.
*        "consistência de faixa de valor
*        IF WA_LOOP_ZADTO-VALOR_DE IS INITIAL OR WA_LOOP_ZADTO-VALOR_ATE IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
*          LEAVE TO SCREEN 9000.
*        ELSEIF WA_LOOP_ZADTO-VALOR_DE GE WA_LOOP_ZADTO-VALOR_ATE.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
*          CLEAR: VALOR_DE, VALOR_ATE.
*          LEAVE TO SCREEN 9000.
*        ELSE.
*          IF SY-TABIX EQ 1.
*            IF WA_LOOP_ZADTO-VALOR_DE GT WA_LOOP_ZADTO-VALOR_ATE.
*              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: VALOR_DE, VALOR_ATE.
*              LEAVE TO SCREEN 9000.
*            ELSE.
*              VALOR_DE = WA_LOOP_ZADTO-VALOR_DE.
*              VALOR_ATE = WA_LOOP_ZADTO-VALOR_ATE.
*            ENDIF.
*          ELSE.
*            IF WA_LOOP_ZADTO-VALOR_DE = VALOR_ATE + '0.01' AND WA_LOOP_ZADTO-VALOR_ATE GT WA_LOOP_ZADTO-VALOR_DE.
*              VALOR_ATE = WA_LOOP_ZADTO-VALOR_ATE.
*            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: VALOR_DE, VALOR_ATE.
*              LEAVE TO SCREEN 9000.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      CLEAR: NIVEL_CHECK, WA_LOOP_ZADTO, VALOR_DE, VALOR_ATE.
*      IT_LOOP_ZADTO = IT_TEMP_ZADTO.
*    ENDLOOP.
*    CLEAR: IT_DATES, IT_LOOP_ZADTO.
*  ENDLOOP.
  CLEAR: it_temp_zadto.





  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zadto INTO wa_key_zadto.

    it_temp_zadto = it_saida_zadto_final.

    DELETE it_temp_zadto
          WHERE bukrs     NE wa_key_zadto-bukrs
            OR bukrs_ate  NE wa_key_zadto-bukrs_ate
            OR dep_resp   NE wa_key_zadto-dep_resp
            OR waers      NE wa_key_zadto-waers
            OR dt_val_ate   LT sy-datum
            OR ( dt_val_ate EQ sy-datum AND
                 hr_val_ate LT sy-uzeit ).

    "ALRS
    LOOP AT it_temp_zadto INTO wa_temp_zadto.
      IF wa_temp_zadto-dt_val_de = wa_temp_zadto-dt_val_ate AND
         wa_temp_zadto-hr_val_de = wa_temp_zadto-hr_val_ate.
        wa_temp_zadto-ck_ant = 'D'.
        MODIFY it_temp_zadto FROM wa_temp_zadto INDEX sy-tabix TRANSPORTING ck_ant.
      ENDIF.
    ENDLOOP.
    DELETE it_temp_zadto WHERE ck_ant = 'D'.

    DESCRIBE TABLE it_temp_zadto LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zadto.

      it_temp_zadto = it_saida_zadto_final.

      DELETE it_temp_zadto
        WHERE bukrs     < wa_key_zadto-bukrs
          OR bukrs_ate > wa_key_zadto-bukrs_ate
          OR dep_resp   NE wa_key_zadto-dep_resp
          OR waers      NE wa_key_zadto-waers
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      "ALRS
      LOOP AT it_temp_zadto INTO wa_temp_zadto.
        IF wa_temp_zadto-dt_val_de = wa_temp_zadto-dt_val_ate AND
           wa_temp_zadto-hr_val_de = wa_temp_zadto-hr_val_ate.
          wa_temp_zadto-ck_ant = 'D'.
          MODIFY it_temp_zadto FROM wa_temp_zadto INDEX sy-tabix TRANSPORTING ck_ant.
        ENDIF.
      ENDLOOP.
      DELETE it_temp_zadto WHERE ck_ant = 'D'.

      SORT it_temp_zadto BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zadto COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zadto LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zadto.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_zinv .



  LOOP AT it_saida_zinv INTO wa_saida_zinv.
    "/Consistência na Estratégia para falta de dados./
    IF wa_saida_zinv-bukrs          IS INITIAL
      OR wa_saida_zinv-bukrs_ate    IS INITIAL
      OR wa_saida_zinv-tipo         IS INITIAL
      OR wa_saida_zinv-aprovador    IS INITIAL
      OR wa_saida_zinv-waers        IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zinv.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_zinv-motivo IS INITIAL AND wa_saida_zinv-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zinv.
      LEAVE TO SCREEN 9000.
    ENDIF.


    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_zinv-ck_ant NE 'X' AND wa_saida_zinv-dt_val_de < sy-datum )
      OR  ( wa_saida_zinv-ck_ant NE 'X' AND wa_saida_zinv-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zinv.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zinv-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zinv.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zinv-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zinv.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab3.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zinv = it_saida_zinv_final.

  SORT it_key_zinv BY bukrs       ASCENDING
                      bukrs_ate   ASCENDING
                      tipo        ASCENDING
                      waers       ASCENDING
                      tp_operacao ASCENDING
                      matnr       ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_zinv COMPARING bukrs bukrs_ate tipo waers tp_operacao matnr.

  LOOP AT it_key_zinv INTO wa_key_zinv.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_zinv = it_saida_zinv_final.
    DELETE it_temp_zinv
       WHERE bukrs     NE wa_key_zinv-bukrs
        OR bukrs_ate   NE wa_key_zinv-bukrs_ate
        OR tipo        NE wa_key_zinv-tipo
        OR waers       NE wa_key_zinv-waers
        OR tp_operacao NE wa_key_zinv-tp_operacao
        OR matnr       NE wa_key_zinv-matnr.

    CLEAR: it_bukrs.
    REFRESH: it_bukrs.

    LOOP AT it_temp_zinv INTO wa_temp_zinv.
      wa_bukrs-bukrs = wa_temp_zinv-bukrs.
      wa_bukrs-bukrs_ate = wa_temp_zinv-bukrs_ate.
      APPEND wa_bukrs TO it_bukrs.
    ENDLOOP.

    SORT it_bukrs BY bukrs bukrs_ate ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_bukrs COMPARING ALL FIELDS.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_bukrs INTO wa_bukrs.
      it_loop_zinv = it_temp_zinv.

      DELETE it_loop_zinv WHERE bukrs      NE wa_bukrs-bukrs
                          AND   bukrs_ate  NE wa_bukrs-bukrs_ate.

      wa_dates-hour  = sy-uzeit + 1.
      DELETE it_loop_zinv
        WHERE dt_val_ate LE sy-datum
        AND   hr_val_ate LT wa_dates-hour.
*        AND   mandt      EQ ''. ALRS

      DELETE it_loop_zinv
       WHERE ( dt_val_ate LT sy-datum OR  dt_val_de GT sy-datum )
       AND   mandt      NE ''.

      DELETE it_loop_zinv
       WHERE dt_val_de  GT sy-datum
       AND   transf_aprov EQ 'P'.

      SORT it_loop_zinv BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zinv INTO wa_loop_zinv.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zinv-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zinv-nivel IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE |Estratégia fora de sequência tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zinv-valor_de IS INITIAL OR wa_loop_zinv-valor_ate IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia sem Faixa de Valor tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zinv-valor_de GE wa_loop_zinv-valor_ate.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Estratégia com Faixa de Valor incorreta tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zinv-valor_de GT wa_loop_zinv-valor_ate.
*              MESSAGE 'Estratégia com faixa de valores errada! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores errada tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_zinv-valor_de.
              valor_ate = wa_loop_zinv-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zinv-valor_de = valor_ate + '0.01' AND wa_loop_zinv-valor_ate GT wa_loop_zinv-valor_de.
              valor_ate = wa_loop_zinv-valor_ate.
            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência! ZGL' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia com faixa de valores fora de sequência tipo { wa_loop_zinv-tipo } usuário { wa_loop_zinv-aprovador }.| TYPE 'W'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zinv, valor_de, valor_ate.
      it_loop_zinv = it_temp_zinv.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zinv.
  ENDLOOP.

*  LOOP AT IT_KEY_ZINV INTO WA_KEY_ZINV.
*
*    "obtendo tabela temporária de acrodo com a chave de estratégia
*    IT_TEMP_ZINV = IT_SAIDA_ZINV_FINAL.
*    DELETE IT_TEMP_ZINV
*      WHERE BUKRS     NE WA_KEY_ZINV-BUKRS
*        OR BUKRS_ATE  NE WA_KEY_ZINV-BUKRS_ATE
*        OR TIPO NE WA_KEY_ZINV-TIPO
*        OR WAERS      NE WA_KEY_ZINV-WAERS
*        OR TP_OPERACAO NE WA_KEY_ZINV-TP_OPERACAO
*        OR MATNR NE WA_KEY_ZINV-MATNR.
*
*    "populando a tabela de datas a serem investigadas
*    CLEAR: IT_DATES.
*
*    LOOP AT IT_TEMP_ZINV INTO WA_TEMP_ZINV.
*      WA_DATES-DATE = WA_TEMP_ZINV-DT_VAL_DE.
*      WA_DATES-HOUR = WA_TEMP_ZINV-HR_VAL_DE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZINV-HR_VAL_DE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZINV-DT_VAL_DE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZINV-DT_VAL_DE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*      WA_DATES-DATE = WA_TEMP_ZINV-DT_VAL_ATE.
*      WA_DATES-HOUR = WA_TEMP_ZINV-HR_VAL_ATE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZINV-HR_VAL_ATE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZINV-DT_VAL_ATE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZINV-DT_VAL_ATE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*    ENDLOOP.
*
*    SORT IT_DATES BY DATE ASCENDING.
*
*    DELETE ADJACENT DUPLICATES FROM IT_DATES COMPARING DATE HOUR.
*
*    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
*    LOOP AT IT_DATES INTO WA_DATES.
*
*      IT_LOOP_ZINV = IT_TEMP_ZINV.
*
*      "IF WA_DATES IS NOT INITIAL.
*      DELETE IT_LOOP_ZINV
*        WHERE ( DT_VAL_DE < WA_DATES-DATE AND DT_VAL_ATE < WA_DATES-DATE )
*          OR ( DT_VAL_DE > WA_DATES-DATE AND DT_VAL_ATE > WA_DATES-DATE ).
*      "ENDIF.
*
*      DELETE IT_LOOP_ZINV
*            WHERE DT_VAL_ATE < WA_DATES-DATE "WA_DATES-DATE > DT_VAL_ATE
*            OR  DT_VAL_DE > WA_DATES-DATE "WA_DATES-DATE < DT_VAL_DE
*            OR ( DT_VAL_DE EQ WA_DATES-DATE AND HR_VAL_DE > WA_DATES-HOUR )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
*            OR ( DT_VAL_ATE EQ WA_DATES-DATE AND HR_VAL_ATE < WA_DATES-HOUR ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).
*
*      SORT IT_LOOP_ZINV BY NIVEL ASCENDING.
*
*      "loop na tabela loop para consistência de nível e faixa de valor
*      LOOP AT IT_LOOP_ZINV INTO WA_LOOP_ZINV.
*        "consistência de nível
*        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
*          EXPORTING
*            CHR = WA_LOOP_ZINV-NIVEL
*          IMPORTING
*            NUM = NIVEL.
*
*        IF WA_LOOP_ZINV-NIVEL IS INITIAL.
*          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
*          LEAVE TO SCREEN 9000.
*        ELSE.
*          IF SY-TABIX EQ 1.
*            IF NIVEL NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: NIVEL, NIVEL_CHECK.
*              LEAVE TO SCREEN 9000.
*            ELSE.
*              NIVEL_CHECK = NIVEL.
*            ENDIF.
*          ELSE.
*            IF NIVEL = NIVEL_CHECK + 1.
*              NIVEL_CHECK = NIVEL.
*            ELSE.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: NIVEL, NIVEL_CHECK.
*              LEAVE TO SCREEN 9000.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*        CLEAR: NIVEL.
*        "consistência de faixa de valor
*        IF WA_LOOP_ZINV-VALOR_DE IS INITIAL OR WA_LOOP_ZINV-VALOR_ATE IS INITIAL.
*          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
*          LEAVE TO SCREEN 9000.
*        ELSEIF WA_LOOP_ZINV-VALOR_DE GE WA_LOOP_ZINV-VALOR_ATE.
*          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
*          CLEAR: VALOR_DE, VALOR_ATE.
*          LEAVE TO SCREEN 9000.
*        ELSE.
*          IF SY-TABIX EQ 1.
*            IF WA_LOOP_ZINV-VALOR_DE GT WA_LOOP_ZINV-VALOR_ATE.
*              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: VALOR_DE, VALOR_ATE.
*              LEAVE TO SCREEN 9000.
*            ELSE.
*              VALOR_DE = WA_LOOP_ZINV-VALOR_DE.
*              VALOR_ATE = WA_LOOP_ZINV-VALOR_ATE.
*            ENDIF.
*          ELSE.
*            IF WA_LOOP_ZINV-VALOR_DE = VALOR_ATE + '0.01' AND WA_LOOP_ZINV-VALOR_ATE GT WA_LOOP_ZINV-VALOR_DE.
*              VALOR_ATE = WA_LOOP_ZINV-VALOR_ATE.
*            ELSE.
*              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
*              CLEAR: VALOR_DE, VALOR_ATE.
*              LEAVE TO SCREEN 9000.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      CLEAR: NIVEL_CHECK, WA_LOOP_ZINV, VALOR_DE, VALOR_ATE.
*      IT_LOOP_ZINV = IT_TEMP_ZINV.
*    ENDLOOP.
*    CLEAR: IT_DATES, IT_LOOP_ZINV.
*  ENDLOOP.
  CLEAR: it_temp_zinv.




  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zinv INTO wa_key_zinv.

    it_temp_zinv = it_saida_zinv_final.

    DELETE it_temp_zinv
      WHERE bukrs       NE wa_key_zinv-bukrs
        OR bukrs_ate    NE wa_key_zinv-bukrs_ate
        OR tipo         NE wa_key_zinv-tipo
        OR waers        NE wa_key_zinv-waers
        OR tp_operacao  NE wa_key_zinv-tp_operacao
        OR matnr        NE wa_key_zinv-matnr
        OR dt_val_ate   LT sy-datum
        OR ( dt_val_ate EQ sy-datum AND
             hr_val_ate LT sy-uzeit ).

    "ALRS
    LOOP AT it_temp_zinv INTO wa_temp_zinv.
      IF wa_temp_zinv-dt_val_de = wa_temp_zinv-dt_val_ate AND
         wa_temp_zinv-hr_val_de = wa_temp_zinv-hr_val_ate.
        wa_temp_zinv-ck_ant = 'D'.
        MODIFY it_temp_zinv FROM wa_temp_zinv INDEX sy-tabix TRANSPORTING ck_ant.
      ENDIF.
    ENDLOOP.
    DELETE it_temp_zinv WHERE ck_ant = 'D'.

    DESCRIBE TABLE it_temp_zinv LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zinv.

      it_temp_zinv = it_saida_zinv_final.

      DELETE it_temp_zinv
        WHERE bukrs       <  wa_key_zinv-bukrs
          OR bukrs_ate    >  wa_key_zinv-bukrs_ate
          OR tipo         NE wa_key_zinv-tipo
          OR waers        NE wa_key_zinv-waers
          OR tp_operacao  NE wa_key_zinv-tp_operacao
          OR matnr        NE wa_key_zinv-matnr
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      "ALRS
      LOOP AT it_temp_zinv INTO wa_temp_zinv.
        IF wa_temp_zinv-dt_val_de = wa_temp_zinv-dt_val_ate AND
           wa_temp_zinv-hr_val_de = wa_temp_zinv-hr_val_ate.
          wa_temp_zinv-ck_ant = 'D'.
          MODIFY it_temp_zinv FROM wa_temp_zinv INDEX sy-tabix TRANSPORTING ck_ant.
        ENDIF.
      ENDLOOP.
      DELETE it_temp_zinv WHERE ck_ant = 'D'.

      SORT it_temp_zinv BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zinv COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zinv LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zinv.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_zfre .



  LOOP AT it_saida_zfre INTO wa_saida_zfre.
    "/Consistência na Estratégia para falta de dados./
    IF   wa_saida_zfre-bukrs          IS INITIAL
      OR wa_saida_zfre-bukrs_ate    IS INITIAL
      OR wa_saida_zfre-aprovador    IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zfre.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_zfre-motivo IS INITIAL AND wa_saida_zfre-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zfre.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência na Estratégia para data retroativa./
    IF    ( wa_saida_zfre-ck_ant NE 'X' AND wa_saida_zfre-dt_val_de < sy-datum )
      OR  ( wa_saida_zfre-ck_ant NE 'X' AND wa_saida_zfre-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zfre.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zfre-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zfre.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zfre-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zfre.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab7.
  ENDIF.
  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zfre = it_saida_zfre_final.

  SORT it_key_zfre BY bukrs       ASCENDING
                      bukrs_ate   ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_zfre COMPARING bukrs bukrs_ate.

  LOOP AT it_key_zfre INTO wa_key_zfre.

    "obtendo tabela temporária de acrodo com a chave de estratégia
    it_temp_zfre = it_saida_zfre_final.
    DELETE it_temp_zfre
      WHERE bukrs       NE wa_key_zfre-bukrs
        OR  bukrs_ate   NE wa_key_zfre-bukrs_ate.

    "populando a tabela de datas a serem investigadas
    CLEAR: it_dates.

    LOOP AT it_temp_zfre INTO wa_temp_zfre.
      wa_dates-date = wa_temp_zfre-dt_val_de.
      wa_dates-hour = wa_temp_zfre-hr_val_de.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_zfre-hr_val_de + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_zfre-dt_val_de + 1.
        ELSE.
          wa_dates-date = wa_temp_zfre-dt_val_de.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
      wa_dates-date = wa_temp_zfre-dt_val_ate.
      wa_dates-hour = wa_temp_zfre-hr_val_ate.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_zfre-hr_val_ate + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_zfre-dt_val_ate + 1.
        ELSE.
          wa_dates-date = wa_temp_zfre-dt_val_ate.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
    ENDLOOP.

    SORT it_dates BY date ASCENDING.

    DELETE ADJACENT DUPLICATES FROM it_dates COMPARING date hour.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_dates INTO wa_dates.

      it_loop_zfre = it_temp_zfre.

      "IF WA_DATES IS NOT INITIAL.
      DELETE it_loop_zfre
        WHERE ( dt_val_de < wa_dates-date AND dt_val_ate < wa_dates-date )
          OR  ( dt_val_de > wa_dates-date AND dt_val_ate > wa_dates-date ).
      "ENDIF.

      DELETE it_loop_zfre
            WHERE dt_val_ate < wa_dates-date "WA_DATES-DATE > DT_VAL_ATE
            OR  dt_val_de > wa_dates-date "WA_DATES-DATE < DT_VAL_DE
            OR ( dt_val_de EQ wa_dates-date AND hr_val_de > wa_dates-hour )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
            OR ( dt_val_ate EQ wa_dates-date AND hr_val_ate < wa_dates-hour ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).

      SORT it_loop_zfre BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zfre INTO wa_loop_zfre.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zfre-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zfre-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zfre-valor_de IS INITIAL OR wa_loop_zfre-valor_ate IS INITIAL.
          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zfre-valor_de GE wa_loop_zfre-valor_ate.
          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zfre-valor_de GT wa_loop_zfre-valor_ate.
              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_zfre-valor_de.
              valor_ate = wa_loop_zfre-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zfre-valor_de = valor_ate + '0.01' AND wa_loop_zfre-valor_ate GT wa_loop_zfre-valor_de.
              valor_ate = wa_loop_zfre-valor_ate.
            ELSE.
              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zfre, valor_de, valor_ate.
      it_loop_zfre = it_temp_zfre.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zfre.
  ENDLOOP.
  CLEAR: it_temp_zfre.




  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zfre INTO wa_key_zfre.

    it_temp_zfre = it_saida_zfre_final.

    DELETE it_temp_zfre
      WHERE bukrs       NE wa_key_zfre-bukrs
        OR bukrs_ate    NE wa_key_zfre-bukrs_ate
        OR dt_val_ate   LT sy-datum
        OR ( dt_val_ate EQ sy-datum AND
             hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_zfre LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zfre.

      it_temp_zfre = it_saida_zfre_final.

      DELETE it_temp_zfre
        WHERE bukrs       <  wa_key_zfre-bukrs
          OR bukrs_ate    >  wa_key_zfre-bukrs_ate
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_zfre BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zfre COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zfre LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zfre.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.

FORM consistencia_zsolv.


  LOOP AT it_saida_zsolov INTO wa_saida_zsolov.
    "/Consistência na Estratégia para falta de dados./
    IF   wa_saida_zsolov-bukrs        IS INITIAL
      OR wa_saida_zsolov-bukrs_ate    IS INITIAL
      OR wa_saida_zsolov-aprovador    IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zsolov.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_zsolov-motivo IS INITIAL AND wa_saida_zsolov-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zsolov.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência na Estratégia para data retroativa./
    IF    ( wa_saida_zsolov-ck_ant NE 'X' AND wa_saida_zsolov-dt_val_de < sy-datum )
      OR  ( wa_saida_zsolov-ck_ant NE 'X' AND wa_saida_zsolov-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zsolov.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_zsolov-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zsolov.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_zsolov-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_zsolov.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab8.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_zsolov = it_saida_zsolov_final.

  SORT it_key_zsolov BY bukrs       ASCENDING
                        bukrs_ate   ASCENDING
                        vkbur       ASCENDING
                        vkbur_ate   ASCENDING
                        tp_venda    ASCENDING
                        tp_venda_ate ASCENDING
                        waers       ASCENDING.


  DELETE ADJACENT DUPLICATES FROM it_key_zsolov COMPARING bukrs bukrs_ate  vkbur vkbur_ate tp_venda tp_venda_ate waers.

  LOOP AT it_key_zsolov INTO wa_key_zsolov.

    "obtendo tabela temporária de acrodo com a chave de estratégia
    it_temp_zsolov = it_saida_zsolov_final.
    DELETE it_temp_zsolov
      WHERE bukrs         NE wa_key_zsolov-bukrs
        OR  bukrs_ate     NE wa_key_zsolov-bukrs_ate
        OR  vkbur         NE wa_key_zsolov-vkbur
        OR  vkbur_ate     NE wa_key_zsolov-vkbur_ate
        OR  tp_venda      NE wa_key_zsolov-tp_venda
        OR  tp_venda_ate  NE wa_key_zsolov-tp_venda_ate
        OR  waers         NE wa_key_zsolov-waers.

    "populando a tabela de datas a serem investigadas
*    CLEAR: IT_DATES.
*
*    LOOP AT IT_TEMP_ZSOLOV INTO WA_TEMP_ZSOLOV.
*      WA_DATES-DATE = WA_TEMP_ZSOLOV-DT_VAL_DE.
*      WA_DATES-HOUR = WA_TEMP_ZSOLOV-HR_VAL_DE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZSOLOV-HR_VAL_DE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZSOLOV-DT_VAL_DE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZSOLOV-DT_VAL_DE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*      WA_DATES-DATE = WA_TEMP_ZSOLOV-DT_VAL_ATE.
*      WA_DATES-HOUR = WA_TEMP_ZSOLOV-HR_VAL_ATE.
*      APPEND WA_DATES TO IT_DATES.
*      IF WA_DATES-DATE NE '99991231' AND WA_DATES-HOUR NE '99991231'.
*        WA_DATES-HOUR = WA_TEMP_ZSOLOV-HR_VAL_ATE + '000001'.
*        IF WA_DATES-HOUR EQ '000000'.
*          WA_DATES-DATE = WA_TEMP_ZSOLOV-DT_VAL_ATE + 1.
*        ELSE.
*          WA_DATES-DATE = WA_TEMP_ZSOLOV-DT_VAL_ATE.
*        ENDIF.
*      ENDIF.
*      APPEND WA_DATES TO IT_DATES.
*      CLEAR WA_DATES.
*    ENDLOOP.
*
*    SORT IT_DATES BY DATE ASCENDING.

*    DELETE ADJACENT DUPLICATES FROM IT_DATES COMPARING DATE HOUR.

    REFRESH: it_bukrs.

    LOOP AT it_temp_zsolov INTO wa_temp_zsolov.
      wa_bukrs-bukrs = wa_temp_zsolov-bukrs.
      wa_bukrs-bukrs_ate = wa_temp_zsolov-bukrs_ate.
      APPEND wa_bukrs TO it_bukrs.
    ENDLOOP.

    SORT it_bukrs BY bukrs bukrs_ate ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_bukrs COMPARING ALL FIELDS.

    LOOP AT it_bukrs INTO wa_bukrs.

      it_loop_zsolov = it_temp_zsolov.

      DELETE it_loop_zglt WHERE bukrs      NE wa_bukrs-bukrs
                         AND   bukrs_ate  NE wa_bukrs-bukrs_ate.

*      DELETE IT_LOOP_ZSOLOV
*        WHERE ( DT_VAL_DE < WA_DATES-DATE AND DT_VAL_ATE < WA_DATES-DATE )
*          OR  ( DT_VAL_DE > WA_DATES-DATE AND DT_VAL_ATE > WA_DATES-DATE ).
*
*      DELETE IT_LOOP_ZSOLOV
*            WHERE DT_VAL_ATE < WA_DATES-DATE
*            OR  DT_VAL_DE > WA_DATES-DATE
*            OR ( DT_VAL_DE EQ WA_DATES-DATE AND HR_VAL_DE > WA_DATES-HOUR )
*            OR ( DT_VAL_ATE EQ WA_DATES-DATE AND HR_VAL_ATE < WA_DATES-HOUR ).
*
      wa_dates-hour  = sy-uzeit + 1.
      DELETE it_loop_zsolov
        WHERE dt_val_ate LE sy-datum
        AND   hr_val_ate LT wa_dates-hour.
*        AND   mandt      EQ ''.

      DELETE it_loop_zsolov
       WHERE ( dt_val_ate LT sy-datum OR  dt_val_de GT sy-datum )
       AND   mandt      NE ''.

      DELETE it_loop_zsolov
       WHERE dt_val_de  GT sy-datum
       AND   transf_aprov EQ 'P'.


      SORT it_loop_zsolov BY nivel waers ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_zsolov INTO wa_loop_zsolov.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_zsolov-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_zsolov-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência Tipo { wa_loop_zsolov-tp_venda } usuário { wa_loop_zsolov-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência Tipo { wa_loop_zsolov-tp_venda } usuário { wa_loop_zsolov-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_zsolov-valor_de IS INITIAL OR wa_loop_zsolov-valor_ate IS INITIAL.
          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_zsolov-valor_de GE wa_loop_zsolov-valor_ate.
          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_zsolov-valor_de GT wa_loop_zsolov-valor_ate.
              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de  = wa_loop_zsolov-valor_de.
              valor_ate = wa_loop_zsolov-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_zsolov-valor_de = valor_ate + '0.01' AND wa_loop_zsolov-valor_ate GT wa_loop_zsolov-valor_de.
              valor_ate = wa_loop_zsolov-valor_ate.
            ELSE.
              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_zsolov, valor_de, valor_ate.
      it_loop_zsolov = it_temp_zsolov.
    ENDLOOP.
    CLEAR: it_dates, it_loop_zsolov.
  ENDLOOP.
  CLEAR: it_temp_zsolov.


  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_zsolov INTO wa_key_zsolov.

    it_temp_zsolov = it_saida_zsolov_final.

    DELETE it_temp_zsolov
      WHERE bukrs       NE wa_key_zsolov-bukrs
        OR bukrs_ate    NE wa_key_zsolov-bukrs_ate
        OR vkbur        NE wa_key_zsolov-vkbur
        OR vkbur_ate    NE wa_key_zsolov-vkbur_ate
        OR tp_venda     NE wa_key_zsolov-tp_venda
        OR tp_venda_ate NE wa_key_zsolov-tp_venda_ate
        OR waers        NE wa_key_zsolov-waers
        OR dt_val_ate   LT sy-datum
        OR ( dt_val_ate EQ sy-datum AND
             hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_zsolov LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_zsolov.

      it_temp_zsolov = it_saida_zsolov_final.

      DELETE it_temp_zsolov
        WHERE bukrs       <  wa_key_zsolov-bukrs
          OR bukrs_ate    >  wa_key_zsolov-bukrs_ate
          OR vkbur        <  wa_key_zsolov-vkbur
          OR vkbur_ate    >  wa_key_zsolov-vkbur_ate
          OR tp_venda     <  wa_key_zsolov-tp_venda
          OR tp_venda_ate >  wa_key_zsolov-tp_venda_ate
          OR waers        NE wa_key_zsolov-waers
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_zsolov BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_zsolov COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_zsolov LINES linha_02.

      IF linha_01 NE linha_02.
*        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        MESSAGE |Estratégias Duplicadas e Divergentes Tipo { wa_key_zsolov-tp_venda } usuário { wa_key_zsolov-aprovador }.| TYPE 'W'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_zsolov.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.
ENDFORM.

FORM consistencia_var_camb .
  LOOP AT it_saida_var_camb INTO wa_saida_var_camb.
    "/Consistência na Estratégia para falta de dados./
    IF   wa_saida_var_camb-bukrs          IS INITIAL
      OR wa_saida_var_camb-bukrs_ate    IS INITIAL
      OR wa_saida_var_camb-aprovador    IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_var_camb.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_var_camb-motivo IS INITIAL AND wa_saida_var_camb-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_var_camb.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "/Consistência na Estratégia para data retroativa./
    IF    ( wa_saida_var_camb-ck_ant NE 'X' AND wa_saida_var_camb-dt_val_de < sy-datum )
      OR  ( wa_saida_var_camb-ck_ant NE 'X' AND wa_saida_var_camb-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_var_camb.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_var_camb-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_var_camb.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_var_camb-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_var_camb.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab9.
  ENDIF.


  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_varcamb = it_saida_varcamb_final.

  SORT it_key_varcamb BY bukrs       ASCENDING
                         bukrs_ate   ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_varcamb COMPARING bukrs bukrs_ate.

  LOOP AT it_key_varcamb INTO wa_key_varcamb.

    "obtendo tabela temporária de acrodo com a chave de estratégia
    it_temp_varcamb = it_saida_varcamb_final.
    DELETE it_temp_varcamb
      WHERE bukrs       NE wa_key_varcamb-bukrs
        OR  bukrs_ate   NE wa_key_varcamb-bukrs_ate.

    "populando a tabela de datas a serem investigadas
    CLEAR: it_dates.

    LOOP AT it_temp_varcamb INTO wa_temp_varcamb.
      wa_dates-date = wa_temp_varcamb-dt_val_de.
      wa_dates-hour = wa_temp_varcamb-hr_val_de.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_varcamb-hr_val_de + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_varcamb-dt_val_de + 1.
        ELSE.
          wa_dates-date = wa_temp_varcamb-dt_val_de.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
      wa_dates-date = wa_temp_varcamb-dt_val_ate.
      wa_dates-hour = wa_temp_varcamb-hr_val_ate.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_varcamb-hr_val_ate + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_varcamb-dt_val_ate + 1.
        ELSE.
          wa_dates-date = wa_temp_varcamb-dt_val_ate.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
    ENDLOOP.

    SORT it_dates BY date ASCENDING.

    DELETE ADJACENT DUPLICATES FROM it_dates COMPARING date hour.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_dates INTO wa_dates.

      it_loop_varcamb = it_temp_varcamb.

      "IF WA_DATES IS NOT INITIAL.
      DELETE it_loop_varcamb
        WHERE ( dt_val_de < wa_dates-date AND dt_val_ate < wa_dates-date )
          OR  ( dt_val_de > wa_dates-date AND dt_val_ate > wa_dates-date ).
      "ENDIF.

      DELETE it_loop_varcamb
            WHERE dt_val_ate < wa_dates-date "WA_DATES-DATE > DT_VAL_ATE
            OR  dt_val_de > wa_dates-date "WA_DATES-DATE < DT_VAL_DE
            OR ( dt_val_de EQ wa_dates-date AND hr_val_de > wa_dates-hour )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
            OR ( dt_val_ate EQ wa_dates-date AND hr_val_ate < wa_dates-hour ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).

      SORT it_loop_varcamb BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_varcamb INTO wa_loop_varcamb.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_varcamb-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_varcamb-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_loop_varcamb-valor_de IS INITIAL OR wa_loop_varcamb-valor_ate IS INITIAL.
          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_loop_varcamb-valor_de GE wa_loop_varcamb-valor_ate.
          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_loop_varcamb-valor_de GT wa_loop_varcamb-valor_ate.
              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_loop_varcamb-valor_de.
              valor_ate = wa_loop_varcamb-valor_ate.
            ENDIF.
          ELSE.
            IF wa_loop_varcamb-valor_de = valor_ate + '0.01' AND wa_loop_varcamb-valor_ate GT wa_loop_varcamb-valor_de.
              valor_ate = wa_loop_varcamb-valor_ate.
            ELSE.
              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_varcamb, valor_de, valor_ate.
      it_loop_varcamb = it_temp_varcamb.
    ENDLOOP.
    CLEAR: it_dates, it_loop_varcamb.
  ENDLOOP.
  CLEAR: it_temp_varcamb.




  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_varcamb INTO wa_key_varcamb.

    it_temp_varcamb = it_saida_varcamb_final.

    DELETE it_temp_varcamb
      WHERE bukrs       NE wa_key_varcamb-bukrs
        OR bukrs_ate    NE wa_key_varcamb-bukrs_ate
        OR dt_val_ate   LT sy-datum
        OR ( dt_val_ate EQ sy-datum AND
             hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_varcamb LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_varcamb.

      it_temp_varcamb = it_saida_varcamb_final.

      DELETE it_temp_varcamb
        WHERE bukrs       <  wa_key_varcamb-bukrs
          OR bukrs_ate    >  wa_key_varcamb-bukrs_ate
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_varcamb BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_varcamb COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_varcamb LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_var_camb.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_data.

  DATA: it_zimp_aprovador  TYPE STANDARD TABLE OF zimp_aprovador,
        it_zglt037         TYPE STANDARD TABLE OF zglt037,
        it_zsdt0141        TYPE STANDARD TABLE OF zsdt0141,
        it_zsdt0152        TYPE STANDARD TABLE OF zsdt0152,
        it_zadto_aprovador TYPE STANDARD TABLE OF zadto_aprovador,
        it_zinv_aprovador  TYPE STANDARD TABLE OF zinv_aprovador,
        it_zlest0156       TYPE STANDARD TABLE OF zlest0156,
        it_zsdt0161        TYPE STANDARD TABLE OF zsdt0161,
        it_zsdt0336        TYPE STANDARD TABLE OF zsdt0336,
        it_zfiwrt0033      TYPE STANDARD TABLE OF zfiwrt0033,
        it_zsdt0385        TYPE STANDARD TABLE OF zsdt0385, " 07.05.2025 - 174338 - RAMON
        it_zmmt0150        TYPE STANDARD TABLE OF zmmt0150,
        wa_zimp_aprovador  TYPE zimp_aprovador,
        wa_zglt037         TYPE zglt037,
        wa_zsdt0141        TYPE zsdt0141,
        wa_zsdt0152        TYPE zsdt0152,
        wa_zadto_aprovador TYPE zadto_aprovador,
        wa_zinv_aprovador  TYPE zinv_aprovador,
        wa_zlest0156       TYPE zlest0156,
        wa_zsdt0161        TYPE zsdt0161,
        wa_zsdt0336        TYPE zsdt0336,
        wa_zfiwrt0033      TYPE zfiwrt0033,
        wa_zsdt0385        TYPE zsdt0385,
        wa_zmmt0150        TYPE zmmt0150.

  IF i_main_tab-pressed_tab = c_main_tab-tab1.

    SELECT * FROM zimp_aprovador INTO TABLE it_zimp_aprovador.
    "Deleta da ZIMP_APROVADOR os registros que não contém na ZIMP agregada
    IF it_saida_zimp_final[] IS NOT INITIAL.
      LOOP AT it_zimp_aprovador INTO wa_zimp_aprovador.
        READ TABLE it_saida_zimp_final WITH KEY bukrs         = wa_zimp_aprovador-bukrs
                                                bukrs_ate     = wa_zimp_aprovador-bukrs_ate
                                                dep_resp      = wa_zimp_aprovador-dep_resp
                                                waers         = wa_zimp_aprovador-waers
                                                nivel         = wa_zimp_aprovador-nivel
                                                aprovador     = wa_zimp_aprovador-aprovador
                                                valor_de      = wa_zimp_aprovador-valor_de
                                                valor_ate     = wa_zimp_aprovador-valor_ate
                                                data_atual    = wa_zimp_aprovador-data_atual
                                                hora_atual    = wa_zimp_aprovador-hora_atual
                                                usuario       = wa_zimp_aprovador-usuario
                                                dt_val_de     = wa_zimp_aprovador-dt_val_de
                                                dt_val_ate    = wa_zimp_aprovador-dt_val_ate
                                                hr_val_de     = wa_zimp_aprovador-hr_val_de
                                                hr_val_ate    = wa_zimp_aprovador-hr_val_ate
                                                motivo        = wa_zimp_aprovador-motivo
                                                transf_aprov  = wa_zimp_aprovador-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zimp_aprovador FROM wa_zimp_aprovador.
        ENDIF.
      ENDLOOP.
      CLEAR: it_zimp_aprovador, wa_zimp_aprovador.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 1 SECONDS.

    SELECT * FROM zimp_aprovador INTO TABLE it_zimp_aprovador.
    "Adiciona na ZIMP os novos registros na ZIMP agregada
    LOOP AT it_saida_zimp_final INTO wa_saida_zimp_final.

      READ TABLE it_zimp_aprovador WITH KEY bukrs         = wa_saida_zimp_final-bukrs
                                            bukrs_ate     = wa_saida_zimp_final-bukrs_ate
                                            dep_resp      = wa_saida_zimp_final-dep_resp
                                            waers         = wa_saida_zimp_final-waers
                                            nivel         = wa_saida_zimp_final-nivel
                                            aprovador     = wa_saida_zimp_final-aprovador
                                            valor_de      = wa_saida_zimp_final-valor_de
                                            valor_ate     = wa_saida_zimp_final-valor_ate
                                            data_atual    = wa_saida_zimp_final-data_atual
                                            hora_atual    = wa_saida_zimp_final-hora_atual
                                            usuario       = wa_saida_zimp_final-usuario
                                            dt_val_de     = wa_saida_zimp_final-dt_val_de
                                            dt_val_ate    = wa_saida_zimp_final-dt_val_ate
                                            hr_val_de     = wa_saida_zimp_final-hr_val_de
                                            hr_val_ate    = wa_saida_zimp_final-hr_val_ate
                                            motivo        = wa_saida_zimp_final-motivo
                                            transf_aprov  = wa_saida_zimp_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zimp_final TO wa_zimp_aprovador.
        wa_zimp_aprovador-data_atual    = sy-datum.
        wa_zimp_aprovador-hora_atual    = sy-uzeit.
        wa_zimp_aprovador-usuario       = sy-uname.
        MODIFY zimp_aprovador FROM wa_zimp_aprovador.
      ENDIF.
      CLEAR: wa_zimp_aprovador.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.

    CLEAR it_zimp_aprovador.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab2.

    SELECT * FROM zglt037 INTO TABLE it_zglt037.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_zglt_final[] IS NOT INITIAL.
      LOOP AT it_zglt037 INTO wa_zglt037.
        READ TABLE it_saida_zglt_final WITH KEY bukrs         = wa_zglt037-bukrs
                                          bukrs_ate     = wa_zglt037-bukrs_ate
                                          dep_resp      = wa_zglt037-dep_resp
                                          pgt_forn      = wa_zglt037-pgt_forn
                                          waers         = wa_zglt037-waers
                                          nivel         = wa_zglt037-nivel
                                          aprovador     = wa_zglt037-aprovador
                                          valor_de      = wa_zglt037-valor_de
                                          valor_ate     = wa_zglt037-valor_ate
                                          data_atual    = wa_zglt037-data_atual
                                          hora_atual    = wa_zglt037-hora_atual
                                          usuario       = wa_zglt037-usuario
                                          dt_val_de     = wa_zglt037-dt_val_de
                                          dt_val_ate    = wa_zglt037-dt_val_ate
                                          hr_val_de     = wa_zglt037-hr_val_de
                                          hr_val_ate    = wa_zglt037-hr_val_ate
                                          motivo        = wa_zglt037-motivo
                                          transf_aprov  = wa_zglt037-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zglt037 FROM wa_zglt037.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zglt037, wa_zglt037.
    SELECT * FROM zglt037 INTO TABLE it_zglt037.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_zglt_final INTO wa_saida_zglt_final.
      READ TABLE it_zglt037 WITH KEY bukrs         = wa_saida_zglt_final-bukrs
                                     bukrs_ate     = wa_saida_zglt_final-bukrs_ate
                                     dep_resp      = wa_saida_zglt_final-dep_resp
                                     pgt_forn      = wa_saida_zglt_final-pgt_forn
                                     waers         = wa_saida_zglt_final-waers
                                     nivel         = wa_saida_zglt_final-nivel
                                     aprovador     = wa_saida_zglt_final-aprovador
                                     valor_de      = wa_saida_zglt_final-valor_de
                                     valor_ate     = wa_saida_zglt_final-valor_ate
                                     data_atual    = wa_saida_zglt_final-data_atual
                                     hora_atual    = wa_saida_zglt_final-hora_atual
                                     usuario       = wa_saida_zglt_final-usuario
                                     dt_val_de     = wa_saida_zglt_final-dt_val_de
                                     dt_val_ate    = wa_saida_zglt_final-dt_val_ate
                                     hr_val_de     = wa_saida_zglt_final-hr_val_de
                                     hr_val_ate    = wa_saida_zglt_final-hr_val_ate
                                     motivo        = wa_saida_zglt_final-motivo
                                     transf_aprov  = wa_saida_zglt_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zglt_final TO wa_zglt037.
        wa_zglt037-data_atual    = sy-datum.
        wa_zglt037-hora_atual    = sy-uzeit.
        wa_zglt037-usuario       = sy-uname.
        MODIFY zglt037 FROM wa_zglt037.
      ENDIF.
      CLEAR: wa_zglt037.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zglt037.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab3.

    SELECT * FROM zinv_aprovador INTO TABLE it_zinv_aprovador.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_zinv_final[] IS NOT INITIAL.
      LOOP AT it_zinv_aprovador INTO wa_zinv_aprovador.
        READ TABLE it_saida_zinv_final WITH KEY bukrs         = wa_zinv_aprovador-bukrs
                                          bukrs_ate     = wa_zinv_aprovador-bukrs_ate
                                          tipo          = wa_zinv_aprovador-tipo
                                          waers         = wa_zinv_aprovador-waers
                                          tp_operacao   = wa_zinv_aprovador-tp_operacao
                                          matnr         = wa_zinv_aprovador-matnr
                                          nivel         = wa_zinv_aprovador-nivel
                                          aprovador     = wa_zinv_aprovador-aprovador
                                          valor_de      = wa_zinv_aprovador-valor_de
                                          valor_ate     = wa_zinv_aprovador-valor_ate
                                          data_atual    = wa_zinv_aprovador-data_atual
                                          hora_atual    = wa_zinv_aprovador-hora_atual
                                          usuario       = wa_zinv_aprovador-usuario
                                          dt_val_de     = wa_zinv_aprovador-dt_val_de
                                          dt_val_ate    = wa_zinv_aprovador-dt_val_ate
                                          hr_val_de     = wa_zinv_aprovador-hr_val_de
                                          hr_val_ate    = wa_zinv_aprovador-hr_val_ate
                                          motivo        = wa_zinv_aprovador-motivo
                                          transf_aprov  = wa_zinv_aprovador-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zinv_aprovador FROM wa_zinv_aprovador.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zinv_aprovador, wa_zinv_aprovador.
    SELECT * FROM zinv_aprovador INTO TABLE it_zinv_aprovador.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_zinv_final INTO wa_saida_zinv_final.
      READ TABLE it_zinv_aprovador WITH KEY bukrs         = wa_saida_zinv_final-bukrs
                                            bukrs_ate     = wa_saida_zinv_final-bukrs_ate
                                            tipo          = wa_saida_zinv_final-tipo
                                            waers         = wa_saida_zinv_final-waers
                                            tp_operacao   = wa_saida_zinv_final-tp_operacao
                                            matnr         = wa_saida_zinv_final-matnr
                                            nivel         = wa_saida_zinv_final-nivel
                                            aprovador     = wa_saida_zinv_final-aprovador
                                            valor_de      = wa_saida_zinv_final-valor_de
                                            valor_ate     = wa_saida_zinv_final-valor_ate
                                            data_atual    = wa_saida_zinv_final-data_atual
                                            hora_atual    = wa_saida_zinv_final-hora_atual
                                            usuario       = wa_saida_zinv_final-usuario
                                            dt_val_de     = wa_saida_zinv_final-dt_val_de
                                            dt_val_ate    = wa_saida_zinv_final-dt_val_ate
                                            hr_val_de     = wa_saida_zinv_final-hr_val_de
                                            hr_val_ate    = wa_saida_zinv_final-hr_val_ate
                                            motivo        = wa_saida_zinv_final-motivo
                                            transf_aprov  = wa_saida_zinv_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zinv_final TO wa_zinv_aprovador.
        wa_zinv_aprovador-data_atual    = sy-datum.
        wa_zinv_aprovador-hora_atual    = sy-uzeit.
        wa_zinv_aprovador-usuario       = sy-uname.
        MODIFY zinv_aprovador FROM wa_zinv_aprovador.
      ENDIF.
      CLEAR: wa_zinv_aprovador.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zinv_aprovador.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab4.

    SELECT * FROM zadto_aprovador INTO TABLE it_zadto_aprovador.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_zadto_final[] IS NOT INITIAL.
      LOOP AT it_zadto_aprovador INTO wa_zadto_aprovador.

        READ TABLE it_saida_zadto_final WITH KEY bukrs         = wa_zadto_aprovador-bukrs
                                                 bukrs_ate     = wa_zadto_aprovador-bukrs_ate
                                                 dep_resp      = wa_zadto_aprovador-dep_resp
                                                 waers         = wa_zadto_aprovador-waers
                                                 nivel         = wa_zadto_aprovador-nivel
                                                 aprovador     = wa_zadto_aprovador-aprovador
                                                 valor_de      = wa_zadto_aprovador-valor_de
                                                 valor_ate     = wa_zadto_aprovador-valor_ate
                                                 data_atual    = wa_zadto_aprovador-data_atual
                                                 hora_atual    = wa_zadto_aprovador-hora_atual
                                                 usuario       = wa_zadto_aprovador-usuario
                                                 dt_val_de     = wa_zadto_aprovador-dt_val_de
                                                 dt_val_ate    = wa_zadto_aprovador-dt_val_ate
                                                 hr_val_de     = wa_zadto_aprovador-hr_val_de
                                                 hr_val_ate    = wa_zadto_aprovador-hr_val_ate
                                                 motivo        = wa_zadto_aprovador-motivo
                                                 transf_aprov  = wa_zadto_aprovador-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zadto_aprovador FROM wa_zadto_aprovador.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zadto_aprovador, wa_zadto_aprovador.
    SELECT * FROM zadto_aprovador INTO TABLE it_zadto_aprovador.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_zadto_final INTO wa_saida_zadto_final.

      READ TABLE it_zadto_aprovador WITH KEY bukrs         = wa_saida_zadto_final-bukrs
                                             bukrs_ate     = wa_saida_zadto_final-bukrs_ate
                                             dep_resp      = wa_saida_zadto_final-dep_resp
                                             waers         = wa_saida_zadto_final-waers
                                             nivel         = wa_saida_zadto_final-nivel
                                             aprovador     = wa_saida_zadto_final-aprovador
                                             valor_de      = wa_saida_zadto_final-valor_de
                                             valor_ate     = wa_saida_zadto_final-valor_ate
                                             data_atual    = wa_saida_zadto_final-data_atual
                                             hora_atual    = wa_saida_zadto_final-hora_atual
                                             usuario       = wa_saida_zadto_final-usuario
                                             dt_val_de     = wa_saida_zadto_final-dt_val_de
                                             dt_val_ate    = wa_saida_zadto_final-dt_val_ate
                                             hr_val_de     = wa_saida_zadto_final-hr_val_de
                                             hr_val_ate    = wa_saida_zadto_final-hr_val_ate
                                             motivo        = wa_saida_zadto_final-motivo
                                             transf_aprov  = wa_saida_zadto_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zadto_final TO wa_zadto_aprovador.
        wa_zadto_aprovador-data_atual    = sy-datum.
        wa_zadto_aprovador-hora_atual    = sy-uzeit.
        wa_zadto_aprovador-usuario       = sy-uname.
        MODIFY zadto_aprovador FROM wa_zadto_aprovador.
      ENDIF.
      CLEAR: wa_zadto_aprovador.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zadto_aprovador.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab5.

    SELECT * FROM zsdt0141 INTO TABLE it_zsdt0141.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_zov_final[] IS NOT INITIAL.
      LOOP AT it_zsdt0141 INTO wa_zsdt0141.
        READ TABLE it_saida_zov_final WITH KEY bukrs         = wa_zsdt0141-bukrs
                                               bukrs_ate     = wa_zsdt0141-bukrs_ate
                                               vkbur         = wa_zsdt0141-vkbur
                                               vkbur_ate     = wa_zsdt0141-vkbur_ate
                                               waers         = wa_zsdt0141-waers
                                               nivel         = wa_zsdt0141-nivel
                                               aprovador     = wa_zsdt0141-aprovador
                                               valor_de      = wa_zsdt0141-valor_de
                                               valor_ate     = wa_zsdt0141-valor_ate
                                               data_atual    = wa_zsdt0141-data_atual
                                               hora_atual    = wa_zsdt0141-hora_atual
                                               usuario       = wa_zsdt0141-usuario
                                               dt_val_de     = wa_zsdt0141-dt_val_de
                                               dt_val_ate    = wa_zsdt0141-dt_val_ate
                                               hr_val_de     = wa_zsdt0141-hr_val_de
                                               hr_val_ate    = wa_zsdt0141-hr_val_ate
                                               motivo        = wa_zsdt0141-motivo
                                               transf_aprov  = wa_zsdt0141-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zsdt0141 FROM wa_zsdt0141.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zsdt0141, wa_zsdt0141.
    SELECT * FROM zsdt0141 INTO TABLE it_zsdt0141.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_zov_final INTO wa_saida_zov_final.
      READ TABLE it_zsdt0141 WITH KEY bukrs         = wa_saida_zov_final-bukrs
                                      bukrs_ate     = wa_saida_zov_final-bukrs_ate
                                      vkbur         = wa_saida_zov_final-vkbur
                                      vkbur_ate     = wa_saida_zov_final-vkbur_ate
                                      waers         = wa_saida_zov_final-waers
                                      nivel         = wa_saida_zov_final-nivel
                                      aprovador     = wa_saida_zov_final-aprovador
                                      valor_de      = wa_saida_zov_final-valor_de
                                      valor_ate     = wa_saida_zov_final-valor_ate
                                      data_atual    = wa_saida_zov_final-data_atual
                                      hora_atual    = wa_saida_zov_final-hora_atual
                                      usuario       = wa_saida_zov_final-usuario
                                      dt_val_de     = wa_saida_zov_final-dt_val_de
                                      dt_val_ate    = wa_saida_zov_final-dt_val_ate
                                      hr_val_de     = wa_saida_zov_final-hr_val_de
                                      hr_val_ate    = wa_saida_zov_final-hr_val_ate
                                      motivo        = wa_saida_zov_final-motivo
                                      transf_aprov  = wa_saida_zov_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zov_final TO wa_zsdt0141.
        wa_zsdt0141-data_atual    = sy-datum.
        wa_zsdt0141-hora_atual    = sy-uzeit.
        wa_zsdt0141-usuario       = sy-uname.
        MODIFY zsdt0141 FROM wa_zsdt0141.
      ENDIF.
      CLEAR: wa_zsdt0141.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zsdt0141.
    "alrs
  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab6.

    SELECT * FROM zsdt0152 INTO TABLE it_zsdt0152.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_lim_final[] IS NOT INITIAL.
      LOOP AT it_zsdt0152 INTO wa_zsdt0152.
        READ TABLE it_saida_lim_final WITH KEY vkorg         = wa_zsdt0152-vkorg
                                               werks         = wa_zsdt0152-werks
                                               werks_ate     = wa_zsdt0152-werks_ate
                                               nivel         = wa_zsdt0152-nivel
                                               aprovador     = wa_zsdt0152-aprovador
                                               valor_de      = wa_zsdt0152-valor_de
                                               valor_ate     = wa_zsdt0152-valor_ate
                                               data_atual    = wa_zsdt0152-data_atual
                                               hora_atual    = wa_zsdt0152-hora_atual
                                               usuario       = wa_zsdt0152-usuario
                                               dt_val_de     = wa_zsdt0152-dt_val_de
                                               dt_val_ate    = wa_zsdt0152-dt_val_ate
                                               hr_val_de     = wa_zsdt0152-hr_val_de
                                               hr_val_ate    = wa_zsdt0152-hr_val_ate
                                               waers         = wa_zsdt0152-waers
                                               motivo        = wa_zsdt0152-motivo
                                               transf_aprov  = wa_zsdt0152-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zsdt0152 FROM wa_zsdt0152.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zsdt0152, wa_zsdt0152.
    SELECT * FROM zsdt0152 INTO TABLE it_zsdt0152.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_lim_final INTO wa_saida_lim_final.
      READ TABLE it_zsdt0152 WITH KEY vkorg         = wa_saida_lim_final-vkorg
                                      werks         = wa_saida_lim_final-werks
                                      werks_ate     = wa_saida_lim_final-werks_ate
                                      nivel         = wa_saida_lim_final-nivel
                                      aprovador     = wa_saida_lim_final-aprovador
                                      valor_de      = wa_saida_lim_final-valor_de
                                      valor_ate     = wa_saida_lim_final-valor_ate
                                      data_atual    = wa_saida_lim_final-data_atual
                                      hora_atual    = wa_saida_lim_final-hora_atual
                                      usuario       = wa_saida_lim_final-usuario
                                      dt_val_de     = wa_saida_lim_final-dt_val_de
                                      dt_val_ate    = wa_saida_lim_final-dt_val_ate
                                      hr_val_de     = wa_saida_lim_final-hr_val_de
                                      hr_val_ate    = wa_saida_lim_final-hr_val_ate
                                      waers         = wa_saida_lim_final-waers
                                      motivo        = wa_saida_lim_final-motivo
                                      transf_aprov  = wa_saida_lim_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_lim_final TO wa_zsdt0152.
        wa_zsdt0152-data_atual    = sy-datum.
        wa_zsdt0152-hora_atual    = sy-uzeit.
        wa_zsdt0152-usuario       = sy-uname.
        MODIFY zsdt0152 FROM wa_zsdt0152.
      ENDIF.
      CLEAR: wa_zsdt0152.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zsdt0152.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab7.

    SELECT * FROM zlest0156 INTO TABLE it_zlest0156.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_zfre_final[] IS NOT INITIAL.
      LOOP AT it_zlest0156 INTO wa_zlest0156.
        READ TABLE it_saida_zfre_final WITH KEY bukrs         = wa_zlest0156-bukrs
                                                bukrs_ate     = wa_zlest0156-bukrs_ate
                                                nivel         = wa_zlest0156-nivel
                                                aprovador     = wa_zlest0156-aprovador
                                                valor_de      = wa_zlest0156-valor_de
                                                valor_ate     = wa_zlest0156-valor_ate
                                                data_atual    = wa_zlest0156-data_atual
                                                hora_atual    = wa_zlest0156-hora_atual
                                                usuario       = wa_zlest0156-usuario
                                                dt_val_de     = wa_zlest0156-dt_val_de
                                                dt_val_ate    = wa_zlest0156-dt_val_ate
                                                hr_val_de     = wa_zlest0156-hr_val_de
                                                hr_val_ate    = wa_zlest0156-hr_val_ate
                                                motivo        = wa_zlest0156-motivo
                                                transf_aprov  = wa_zlest0156-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zlest0156 FROM wa_zlest0156.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zlest0156, wa_zlest0156.
    SELECT * FROM zlest0156 INTO TABLE it_zlest0156.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_zfre_final INTO wa_saida_zfre_final.
      READ TABLE it_zlest0156 WITH KEY bukrs         = wa_saida_zfre_final-bukrs
                                       bukrs_ate     = wa_saida_zfre_final-bukrs_ate
                                       nivel         = wa_saida_zfre_final-nivel
                                       aprovador     = wa_saida_zfre_final-aprovador
                                       valor_de      = wa_saida_zfre_final-valor_de
                                       valor_ate     = wa_saida_zfre_final-valor_ate
                                       data_atual    = wa_saida_zfre_final-data_atual
                                       hora_atual    = wa_saida_zfre_final-hora_atual
                                       usuario       = wa_saida_zfre_final-usuario
                                       dt_val_de     = wa_saida_zfre_final-dt_val_de
                                       dt_val_ate    = wa_saida_zfre_final-dt_val_ate
                                       hr_val_de     = wa_saida_zfre_final-hr_val_de
                                       hr_val_ate    = wa_saida_zfre_final-hr_val_ate
                                       motivo        = wa_saida_zfre_final-motivo
                                       transf_aprov  = wa_saida_zfre_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zfre_final TO wa_zlest0156.
        wa_zlest0156-data_atual    = sy-datum.
        wa_zlest0156-hora_atual    = sy-uzeit.
        wa_zlest0156-usuario       = sy-uname.
        MODIFY zlest0156 FROM wa_zlest0156.
      ENDIF.
      CLEAR: wa_zlest0156.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zlest0156.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab8.

    SELECT * FROM zsdt0161 INTO TABLE it_zsdt0161.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_zsolov_final[] IS NOT INITIAL.
      LOOP AT it_zsdt0161 INTO wa_zsdt0161.
        READ TABLE it_saida_zsolov_final WITH KEY bukrs         = wa_zsdt0161-bukrs
                                                  bukrs_ate     = wa_zsdt0161-bukrs_ate
                                                  nivel         = wa_zsdt0161-nivel
                                                  aprovador     = wa_zsdt0161-aprovador
                                                  valor_de      = wa_zsdt0161-valor_de
                                                  valor_ate     = wa_zsdt0161-valor_ate
                                                  data_atual    = wa_zsdt0161-data_atual
                                                  hora_atual    = wa_zsdt0161-hora_atual
                                                  usuario       = wa_zsdt0161-usuario
                                                  dt_val_de     = wa_zsdt0161-dt_val_de
                                                  dt_val_ate    = wa_zsdt0161-dt_val_ate
                                                  hr_val_de     = wa_zsdt0161-hr_val_de
                                                  hr_val_ate    = wa_zsdt0161-hr_val_ate
                                                  motivo        = wa_zsdt0161-motivo
                                                  transf_aprov  = wa_zsdt0161-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zsdt0161 FROM wa_zsdt0161.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zsdt0161, wa_zsdt0161.
    SELECT * FROM zsdt0161 INTO TABLE it_zsdt0161.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_zsolov_final INTO wa_saida_zsolov_final.
      READ TABLE it_zsdt0161 WITH KEY  bukrs         = wa_saida_zsolov_final-bukrs
                                       bukrs_ate     = wa_saida_zsolov_final-bukrs_ate
                                       nivel         = wa_saida_zsolov_final-nivel
                                       aprovador     = wa_saida_zsolov_final-aprovador
                                       valor_de      = wa_saida_zsolov_final-valor_de
                                       valor_ate     = wa_saida_zsolov_final-valor_ate
                                       data_atual    = wa_saida_zsolov_final-data_atual
                                       hora_atual    = wa_saida_zsolov_final-hora_atual
                                       usuario       = wa_saida_zsolov_final-usuario
                                       dt_val_de     = wa_saida_zsolov_final-dt_val_de
                                       dt_val_ate    = wa_saida_zsolov_final-dt_val_ate
                                       hr_val_de     = wa_saida_zsolov_final-hr_val_de
                                       hr_val_ate    = wa_saida_zsolov_final-hr_val_ate
                                       motivo        = wa_saida_zsolov_final-motivo
                                       transf_aprov  = wa_saida_zsolov_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_zsolov_final TO wa_zsdt0161.
        wa_zsdt0161-data_atual    = sy-datum.
        wa_zsdt0161-hora_atual    = sy-uzeit.
        wa_zsdt0161-usuario       = sy-uname.
        MODIFY zsdt0161 FROM wa_zsdt0161.
      ENDIF.
      CLEAR: wa_zsdt0161.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zsdt0161.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab9.
    SELECT * FROM zmmt0150 INTO TABLE it_zmmt0150.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_varcamb_final[] IS NOT INITIAL.
      LOOP AT it_zmmt0150 INTO wa_zmmt0150.
        READ TABLE it_saida_varcamb_final WITH KEY bukrs          = wa_zmmt0150-bukrs
                                                    bukrs_ate     = wa_zmmt0150-bukrs_ate
                                                    nivel         = wa_zmmt0150-nivel
                                                    aprovador     = wa_zmmt0150-aprovador
                                                    valor_de      = wa_zmmt0150-valor_de
                                                    valor_ate     = wa_zmmt0150-valor_ate
                                                    data_atual    = wa_zmmt0150-data_atual
                                                    hora_atual    = wa_zmmt0150-hora_atual
                                                    usuario       = wa_zmmt0150-usuario
                                                    dt_val_de     = wa_zmmt0150-dt_val_de
                                                    dt_val_ate    = wa_zmmt0150-dt_val_ate
                                                    hr_val_de     = wa_zmmt0150-hr_val_de
                                                    hr_val_ate    = wa_zmmt0150-hr_val_ate
                                                    motivo        = wa_zmmt0150-motivo
                                                    transf_aprov  = wa_zmmt0150-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zmmt0150 FROM wa_zmmt0150.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CLEAR: it_zmmt0150, wa_zmmt0150.
    SELECT * FROM zmmt0150 INTO TABLE it_zmmt0150.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_varcamb_final INTO wa_saida_varcamb_final.
      READ TABLE it_zmmt0150 WITH KEY bukrs         = wa_saida_varcamb_final-bukrs
                                      bukrs_ate     = wa_saida_varcamb_final-bukrs_ate
                                      nivel         = wa_saida_varcamb_final-nivel
                                      aprovador     = wa_saida_varcamb_final-aprovador
                                      valor_de      = wa_saida_varcamb_final-valor_de
                                      valor_ate     = wa_saida_varcamb_final-valor_ate
                                      data_atual    = wa_saida_varcamb_final-data_atual
                                      hora_atual    = wa_saida_varcamb_final-hora_atual
                                      usuario       = wa_saida_varcamb_final-usuario
                                      dt_val_de     = wa_saida_varcamb_final-dt_val_de
                                      dt_val_ate    = wa_saida_varcamb_final-dt_val_ate
                                      hr_val_de     = wa_saida_varcamb_final-hr_val_de
                                      hr_val_ate    = wa_saida_varcamb_final-hr_val_ate
                                      motivo        = wa_saida_varcamb_final-motivo
                                      transf_aprov  = wa_saida_varcamb_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_varcamb_final TO wa_zmmt0150.
        wa_zmmt0150-data_atual    = sy-datum.
        wa_zmmt0150-hora_atual    = sy-uzeit.
        wa_zmmt0150-usuario       = sy-uname.
        MODIFY zmmt0150 FROM wa_zmmt0150.
      ENDIF.
      CLEAR: wa_zmmt0150.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zmmt0150.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab11.

    SELECT * FROM zsdt0336 INTO TABLE it_zsdt0336.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_isencao_final[] IS NOT INITIAL.
      LOOP AT it_zsdt0336 INTO wa_zsdt0336.
        READ TABLE it_saida_isencao_final WITH KEY bukrs        = wa_zsdt0336-bukrs
                                                  bukrs_ate     = wa_zsdt0336-bukrs_ate
                                                  nivel         = wa_zsdt0336-nivel
                                                  aprovador     = wa_zsdt0336-aprovador
                                                  valor_de      = wa_zsdt0336-valor_de
                                                  valor_ate     = wa_zsdt0336-valor_ate
                                                  data_atual    = wa_zsdt0336-data_atual
                                                  hora_atual    = wa_zsdt0336-hora_atual
                                                  usuario       = wa_zsdt0336-usuario
                                                  dt_val_de     = wa_zsdt0336-dt_val_de
                                                  dt_val_ate    = wa_zsdt0336-dt_val_ate
                                                  hr_val_de     = wa_zsdt0336-hr_val_de
                                                  hr_val_ate    = wa_zsdt0336-hr_val_ate
                                                  motivo        = wa_zsdt0336-motivo
                                                  transf_aprov  = wa_zsdt0336-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zsdt0336 FROM wa_zsdt0336.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ELSEIF it_saida_isencao_final[] IS INITIAL AND it_zsdt0336 IS NOT INITIAL .
      DELETE zsdt0336 FROM TABLE it_zsdt0336.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CLEAR: it_zsdt0336, wa_zsdt0336.
    SELECT * FROM zsdt0336 INTO TABLE it_zsdt0336.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_isencao_final INTO wa_saida_isencao_final.
      READ TABLE it_zsdt0336 WITH KEY  bukrs         = wa_saida_isencao_final-bukrs
                                       bukrs_ate     = wa_saida_isencao_final-bukrs_ate
                                       nivel         = wa_saida_isencao_final-nivel
                                       aprovador     = wa_saida_isencao_final-aprovador
                                       valor_de      = wa_saida_isencao_final-valor_de
                                       valor_ate     = wa_saida_isencao_final-valor_ate
                                       data_atual    = wa_saida_isencao_final-data_atual
                                       hora_atual    = wa_saida_isencao_final-hora_atual
                                       usuario       = wa_saida_isencao_final-usuario
                                       dt_val_de     = wa_saida_isencao_final-dt_val_de
                                       dt_val_ate    = wa_saida_isencao_final-dt_val_ate
                                       hr_val_de     = wa_saida_isencao_final-hr_val_de
                                       hr_val_ate    = wa_saida_isencao_final-hr_val_ate
                                       motivo        = wa_saida_isencao_final-motivo
                                       transf_aprov  = wa_saida_isencao_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_isencao_final TO wa_zsdt0336.
        wa_zsdt0336-data_atual    = sy-datum.
        wa_zsdt0336-hora_atual    = sy-uzeit.
        wa_zsdt0336-usuario       = sy-uname.
        MODIFY zsdt0336 FROM wa_zsdt0336.
      ENDIF.
      CLEAR: wa_zsdt0336.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zsdt0336.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab12."150184 CS2024000781 Aprovações ZNFW - PSA

    SELECT * FROM zfiwrt0033 INTO TABLE it_zfiwrt0033.
    IF it_saida_operznfw_final[] IS NOT INITIAL.
      LOOP AT it_zfiwrt0033 INTO wa_zfiwrt0033.
        READ TABLE it_saida_operznfw_final[] WITH KEY   dep_resp      = wa_zfiwrt0033-dep_resp
                            nivel         = wa_zfiwrt0033-nivel
                            aprovador     = wa_zfiwrt0033-aprovador
                            data_atual    = wa_zfiwrt0033-data_atual
                            hora_atual    = wa_zfiwrt0033-hora_atual
                            usuario       = wa_zfiwrt0033-usuario
                            dt_val_de     = wa_zfiwrt0033-dt_val_de
                            dt_val_ate    = wa_zfiwrt0033-dt_val_ate
                            hr_val_de     = wa_zfiwrt0033-hr_val_de
                            hr_val_ate    = wa_zfiwrt0033-hr_val_ate
                            motivo        = wa_zfiwrt0033-motivo
                            transf_aprov  = wa_zfiwrt0033-transf_aprov
                                                  TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          DELETE zfiwrt0033 FROM wa_zfiwrt0033.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ELSEIF it_saida_operznfw_final[] IS INITIAL AND it_zfiwrt0033 IS NOT INITIAL .
      DELETE zfiwrt0033 FROM TABLE it_zfiwrt0033.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CLEAR: it_zfiwrt0033, wa_zfiwrt0033.
    SELECT * FROM zfiwrt0033 INTO TABLE it_zfiwrt0033.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_operznfw_final[] INTO wa_saida_operznfw_final.
      READ TABLE it_saida_operznfw_final[] WITH KEY   dep_resp      = wa_zfiwrt0033-dep_resp
                          nivel         = wa_zfiwrt0033-nivel
                          aprovador     = wa_zfiwrt0033-aprovador
                          data_atual    = wa_zfiwrt0033-data_atual
                          hora_atual    = wa_zfiwrt0033-hora_atual
                          usuario       = wa_zfiwrt0033-usuario
                          dt_val_de     = wa_zfiwrt0033-dt_val_de
                          dt_val_ate    = wa_zfiwrt0033-dt_val_ate
                          hr_val_de     = wa_zfiwrt0033-hr_val_de
                          hr_val_ate    = wa_zfiwrt0033-hr_val_ate
                          motivo        = wa_zfiwrt0033-motivo
                          transf_aprov  = wa_zfiwrt0033-transf_aprov
                                                TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
*        IF wa_saida_operznfw_final-CK_ANT = abap_false.
        MOVE-CORRESPONDING wa_saida_operznfw_final TO wa_zfiwrt0033.
        wa_zfiwrt0033-data_atual    = sy-datum.
        wa_zfiwrt0033-hora_atual    = sy-uzeit.
        wa_zfiwrt0033-usuario       = sy-uname.
        MODIFY zfiwrt0033 FROM wa_zfiwrt0033.
*        ENDIF.

      ENDIF.
      CLEAR: wa_zfiwrt0033.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zfiwrt0033.

    " 07.05.2025 - 174338 - RAMON -->
  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab13.

    SELECT * FROM zsdt0385 INTO TABLE it_zsdt0385.
    "Deleta da tabela original os registros que sofreram alteração na it_saida
    IF it_saida_checklist_final[] IS NOT INITIAL.
      LOOP AT it_zsdt0385 INTO wa_zsdt0385.
        READ TABLE it_saida_checklist_final WITH KEY bukrs      = wa_zsdt0385-bukrs
                                                  bukrs_ate     = wa_zsdt0385-bukrs_ate
                                                  vkbur         = wa_zsdt0385-vkbur
                                                  vkbur_ate     = wa_zsdt0385-vkbur_ate
                                                  nivel         = wa_zsdt0385-nivel
                                                  aprovador     = wa_zsdt0385-aprovador
                                                  data_atual    = wa_zsdt0385-data_atual
                                                  hora_atual    = wa_zsdt0385-hora_atual
                                                  usuario       = wa_zsdt0385-usuario
                                                  dt_val_de     = wa_zsdt0385-dt_val_de
                                                  dt_val_ate    = wa_zsdt0385-dt_val_ate
                                                  hr_val_de     = wa_zsdt0385-hr_val_de
                                                  hr_val_ate    = wa_zsdt0385-hr_val_ate
                                                  motivo        = wa_zsdt0385-motivo
                                                  transf_aprov  = wa_zsdt0385-transf_aprov TRANSPORTING NO FIELDS.

        IF sy-subrc NE 0.
          DELETE zsdt0385 FROM wa_zsdt0385.
        ENDIF.
      ENDLOOP.
      COMMIT WORK.
      WAIT UP TO 1 SECONDS.
    ELSEIF it_saida_checklist_final[] IS INITIAL AND it_zsdt0385 IS NOT INITIAL .
      DELETE zsdt0385 FROM TABLE it_zsdt0385.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CLEAR: it_zsdt0385, wa_zsdt0385.
    SELECT * FROM zsdt0385 INTO TABLE it_zsdt0385.
    "Adiciona na tabela original os novos registros da it_saida
    LOOP AT it_saida_checklist_final INTO wa_saida_checklist_final.
      READ TABLE it_zsdt0385 WITH KEY  bukrs         = wa_saida_checklist_final-bukrs
                                       bukrs_ate     = wa_saida_checklist_final-bukrs_ate
                                       nivel         = wa_saida_checklist_final-nivel
                                       aprovador     = wa_saida_checklist_final-aprovador
                                       data_atual    = wa_saida_checklist_final-data_atual
                                       hora_atual    = wa_saida_checklist_final-hora_atual
                                       usuario       = wa_saida_checklist_final-usuario
                                       dt_val_de     = wa_saida_checklist_final-dt_val_de
                                       dt_val_ate    = wa_saida_checklist_final-dt_val_ate
                                       hr_val_de     = wa_saida_checklist_final-hr_val_de
                                       hr_val_ate    = wa_saida_checklist_final-hr_val_ate
                                       motivo        = wa_saida_checklist_final-motivo
                                       transf_aprov  = wa_saida_checklist_final-transf_aprov TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING wa_saida_checklist_final TO wa_zsdt0385.
        wa_zsdt0385-data_atual    = sy-datum.
        wa_zsdt0385-hora_atual    = sy-uzeit.
        wa_zsdt0385-usuario       = sy-uname.
        MODIFY zsdt0385 FROM wa_zsdt0385.
      ENDIF.
      CLEAR: wa_zsdt0385.
    ENDLOOP.
    COMMIT WORK.
    WAIT UP TO 1 SECONDS.
    CLEAR it_zsdt0385.



    " 07.05.2025 - 174338 - RAMON --<
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGREGA_ESTRATEGIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM agrega_estrategias.

  IF i_main_tab-pressed_tab = c_main_tab-tab1.

    "Agrega seleção zimp com modificação do usuário e complementar da seleção zimp
    CLEAR: it_saida_zimp_final.
    MOVE-CORRESPONDING it_saida_zimp TO it_saida_zimp_final.

    LOOP AT it_saida_zimp_c INTO wa_saida_zimp_c.
      APPEND wa_saida_zimp_c TO it_saida_zimp_final.
      CLEAR: wa_saida_zimp_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab2.

    "Agrega seleção zglt com modificação do usuário e complementar da seleção zglt
    CLEAR: it_saida_zglt_final.
    MOVE-CORRESPONDING it_saida_zglt TO it_saida_zglt_final.

    LOOP AT it_saida_zglt_c INTO wa_saida_zglt_c.
      APPEND wa_saida_zglt_c TO it_saida_zglt_final.
      CLEAR: wa_saida_zglt_c.
    ENDLOOP.
  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab3.

    "Agrega seleção zinv com modificação do usuário e complementar da seleção zinv
    CLEAR: it_saida_zinv_final.
    MOVE-CORRESPONDING it_saida_zinv TO it_saida_zinv_final.

    LOOP AT it_saida_zinv_c INTO wa_saida_zinv_c.
      APPEND wa_saida_zinv_c TO it_saida_zinv_final.
      CLEAR: wa_saida_zinv_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab4.

    "Agrega seleção zadto com modificação do usuário e complementar da seleção zadto
    CLEAR: it_saida_zadto_final.
    MOVE-CORRESPONDING it_saida_zadto TO it_saida_zadto_final.

    LOOP AT it_saida_zadto_c INTO wa_saida_zadto_c.
      APPEND wa_saida_zadto_c TO it_saida_zadto_final.
      CLEAR: wa_saida_zadto_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab5.

    "Agrega seleção zov com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_zov_final.
    MOVE-CORRESPONDING it_saida_zov TO it_saida_zov_final.

    LOOP AT it_saida_zov_c INTO wa_saida_zov_c.
      APPEND wa_saida_zov_c TO it_saida_zov_final.
      CLEAR: wa_saida_zov_c.
    ENDLOOP.
  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab6.

    "Agrega seleção zov com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_lim_final.
    MOVE-CORRESPONDING it_saida_lim TO it_saida_lim_final.

    LOOP AT it_saida_lim_c INTO wa_saida_lim_c.
      APPEND wa_saida_lim_c TO it_saida_lim_final.
      CLEAR: wa_saida_lim_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab7.

    "Agrega seleção ZFRE com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_zfre_final.
    MOVE-CORRESPONDING it_saida_zfre TO it_saida_zfre_final.

    LOOP AT it_saida_zfre_c INTO wa_saida_zfre_c.
      APPEND wa_saida_zfre_c TO it_saida_zfre_final.
      CLEAR: wa_saida_zfre_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab8.

    "Agrega seleção ZSOLOV com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_zsolov_final.
    MOVE-CORRESPONDING it_saida_zsolov TO it_saida_zsolov_final.

    LOOP AT it_saida_zsolov_c INTO wa_saida_zsolov_c.
      APPEND wa_saida_zsolov_c TO it_saida_zsolov_final.
      CLEAR: wa_saida_zsolov_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab9.

    "Agrega seleção VARCAMB com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_varcamb_final.
    MOVE-CORRESPONDING it_saida_var_camb TO it_saida_varcamb_final.

    LOOP AT it_saida_var_camb_c INTO wa_saida_var_camb_c .
      APPEND wa_saida_var_camb_c  TO it_saida_varcamb_final.
      CLEAR: wa_saida_var_camb_c .
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab11.

    "Agrega seleção ZSOLOV com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_isencao_final.
    MOVE-CORRESPONDING it_saida_isencao TO it_saida_isencao_final.

    LOOP AT it_saida_isencao_c INTO wa_saida_isencao_c.
      APPEND wa_saida_isencao_c TO it_saida_isencao_final.
      CLEAR: wa_saida_isencao_c.
    ENDLOOP.

  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab12. "150184 CS2024000781 Aprovações ZNFW - PSA

    "Agrega seleção operznfw com modificação do usuário e complementar da seleção zOV
    CLEAR: it_saida_operznfw_final.
    MOVE-CORRESPONDING it_saida_operznfw TO it_saida_operznfw_final.

    LOOP AT it_saida_operznfw_c INTO wa_saida_operznfw_c .
      APPEND wa_saida_operznfw_c  TO it_saida_operznfw_final.
      CLEAR: wa_saida_operznfw_c .
    ENDLOOP.

    " 07.05.2025 - 174338 - RAMON -->
  ELSEIF i_main_tab-pressed_tab = c_main_tab-tab13.

    CLEAR: it_saida_checklist_final.
    MOVE-CORRESPONDING it_saida_checklist TO it_saida_checklist_final.

    LOOP AT it_saida_checklist_c INTO wa_saida_checklist_c .
      APPEND wa_saida_checklist_c  TO it_saida_checklist_final.
      CLEAR: wa_saida_checklist_c .
    ENDLOOP.

    " 07.05.2025 - 174338 - RAMON --<


  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form consistencia_isencao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM consistencia_isencao .

  LOOP AT it_saida_isencao INTO wa_saida_isencao.

    IF wa_saida_isencao-mandt IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    "/Consistência na Estratégia para falta de dados./
    IF   wa_saida_isencao-bukrs        IS INITIAL
      OR wa_saida_isencao-bukrs_ate    IS INITIAL
      OR wa_saida_isencao-aprovador    IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_isencao.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_isencao-motivo IS INITIAL AND wa_saida_isencao-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_isencao.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência na Estratégia para data retroativa./
    IF    ( wa_saida_isencao-ck_ant NE 'X' AND wa_saida_isencao-dt_val_de < sy-datum )
      OR  ( wa_saida_isencao-ck_ant NE 'X' AND wa_saida_isencao-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_isencao.
      LEAVE TO SCREEN 9000.
    ENDIF.

    IF wa_saida_isencao-tp_negocio_de IS INITIAL OR wa_saida_isencao-tp_negocio_ate IS INITIAL.
      MESSAGE 'Não é´possível salvar, preencha o tipo de negócio' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_isencao.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_isencao-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_isencao.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_isencao-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_isencao.
      LEAVE TO SCREEN 9000.
    ENDIF.

  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab11.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_isencao = it_saida_isencao_final.

  SORT it_key_isencao BY bukrs       ASCENDING
                        bukrs_ate   ASCENDING
                        vkbur       ASCENDING
                        vkbur_ate   ASCENDING
                        tp_negocio_de    ASCENDING
                        tp_negocio_ate ASCENDING.
*                        waers       ASCENDING. "bug 189279 - SMC - 12-09-2025


  DELETE ADJACENT DUPLICATES FROM it_key_isencao COMPARING bukrs bukrs_ate  vkbur vkbur_ate tp_negocio_de tp_negocio_ate. " waers. "bug 189279 - SMC - 12-09-2025

  LOOP AT it_key_isencao INTO wa_key_isencao.

    "obtendo tabela temporária de acrodo com a chave de estratégia
    it_temp_isencao = it_saida_isencao_final.
    DELETE it_temp_isencao
      WHERE bukrs         NE wa_key_isencao-bukrs
        OR  bukrs_ate     NE wa_key_isencao-bukrs_ate
        OR  vkbur         NE wa_key_isencao-vkbur
        OR  vkbur_ate     NE wa_key_isencao-vkbur_ate
        OR  tp_negocio_de NE wa_key_isencao-tp_negocio_de
        OR  tp_negocio_ate NE wa_key_isencao-tp_negocio_ate.
*        OR  waers         NE wa_key_isencao-waers."bug 189279 - SMC - 12-09-2025

    REFRESH: it_bukrs.

*    LOOP AT it_temp_isencao INTO wa_temp_isencao.
*      wa_bukrs-bukrs = wa_temp_isencao-bukrs.
*      wa_bukrs-bukrs_ate = wa_temp_isencao-bukrs_ate.
*      APPEND wa_bukrs TO it_bukrs.
*    ENDLOOP.
*
*    SORT it_bukrs BY bukrs bukrs_ate ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM it_bukrs COMPARING ALL FIELDS.

    CLEAR: it_dates.

    LOOP AT it_temp_isencao INTO wa_temp_isencao.
      wa_dates-date = wa_temp_isencao-dt_val_de.
      wa_dates-hour = wa_temp_isencao-hr_val_de.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_isencao-hr_val_de + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_isencao-dt_val_de + 1.
        ELSE.
          wa_dates-date = wa_temp_isencao-dt_val_de.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
      wa_dates-date = wa_temp_isencao-dt_val_ate.
      wa_dates-hour = wa_temp_isencao-hr_val_ate.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_isencao-hr_val_ate + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_isencao-dt_val_ate + 1.
        ELSE.
          wa_dates-date = wa_temp_isencao-dt_val_ate.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
    ENDLOOP.

    SORT it_dates BY date ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_dates COMPARING date hour.

    LOOP AT it_dates INTO wa_dates.

      it_loop_isencao = it_temp_isencao.

      DELETE it_loop_isencao
        WHERE ( dt_val_de < wa_dates-date AND dt_val_ate < wa_dates-date )
          OR ( dt_val_de > wa_dates-date AND dt_val_ate > wa_dates-date ).

      DELETE it_loop_isencao
       WHERE dt_val_ate < wa_dates-date "WA_DATES-DATE > DT_VAL_ATE
       OR  dt_val_de > wa_dates-date "WA_DATES-DATE < DT_VAL_DE
       OR ( dt_val_de EQ wa_dates-date AND hr_val_de > wa_dates-hour )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
       OR ( dt_val_ate EQ wa_dates-date AND hr_val_ate < wa_dates-hour ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).

      SORT it_loop_isencao BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_isencao INTO wa_temp_isencao.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_temp_isencao-nivel
          IMPORTING
            num = nivel.

        IF wa_temp_isencao-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
        "consistência de faixa de valor
        IF wa_temp_isencao-valor_de IS INITIAL OR wa_temp_isencao-valor_ate IS INITIAL.
          MESSAGE 'Estratégia sem Faixa de Valor!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSEIF wa_temp_isencao-valor_de GE wa_temp_isencao-valor_ate.
          MESSAGE 'Estratégia com Faixa de Valor incorreta!' TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: valor_de, valor_ate.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF wa_temp_isencao-valor_de GT wa_temp_isencao-valor_ate.
              MESSAGE 'Estratégia com faixa de valores errada!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ELSE.
              valor_de = wa_temp_isencao-valor_de.
              valor_ate = wa_temp_isencao-valor_ate.
            ENDIF.
          ELSE.
            IF wa_temp_isencao-valor_de = valor_ate + '0.01' AND wa_temp_isencao-valor_ate GT wa_temp_isencao-valor_de.
              valor_ate = wa_temp_isencao-valor_ate.
            ELSE.
              MESSAGE 'Estratégia com faixa de valores fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: valor_de, valor_ate.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_isencao, valor_de, valor_ate.
      it_loop_isencao = it_temp_isencao.
    ENDLOOP.
    CLEAR: it_dates, it_loop_isencao.
  ENDLOOP.
  CLEAR: it_temp_isencao.


  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_isencao INTO wa_key_isencao.

    it_temp_isencao = it_saida_isencao_final.

    DELETE it_temp_isencao
      WHERE bukrs       NE wa_key_isencao-bukrs
        OR bukrs_ate    NE wa_key_isencao-bukrs_ate
        OR vkbur        NE wa_key_isencao-vkbur
        OR vkbur_ate    NE wa_key_isencao-vkbur_ate
        OR tp_negocio_de NE wa_key_isencao-tp_negocio_de
        OR tp_negocio_ate NE wa_key_isencao-tp_negocio_ate
*        OR waers        NE wa_key_isencao-waers "bug 189279 - SMC - 12-09-2025
        OR dt_val_ate   LT sy-datum
        OR ( dt_val_ate EQ sy-datum AND
             hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_isencao LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_isencao.

      it_temp_isencao = it_saida_isencao_final.

      DELETE it_temp_isencao
        WHERE bukrs       <  wa_key_isencao-bukrs
          OR bukrs_ate    >  wa_key_isencao-bukrs_ate
          OR vkbur        <  wa_key_isencao-vkbur
          OR vkbur_ate    >  wa_key_isencao-vkbur_ate
          OR tp_negocio_de <  wa_key_isencao-tp_negocio_de
          OR tp_negocio_ate >  wa_key_isencao-tp_negocio_ate
*          OR waers        NE wa_key_isencao-waers "bug 189279 - SMC - 12-09-2025
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_isencao BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_isencao COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_isencao LINES linha_02.

      IF linha_01 NE linha_02.
*        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        MESSAGE |Estratégias Duplicadas e Divergentes Tipo { wa_key_isencao-tp_negocio_de } usuário { wa_key_isencao-aprovador }.| TYPE 'W'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_isencao.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.

ENDFORM.

FORM consistencia_operznfw.

  LOOP AT it_saida_operznfw INTO wa_saida_operznfw.
    "/Consistência na Estratégia para falta de dados./
    IF   wa_saida_operznfw-dep_resp          IS INITIAL
      OR wa_saida_operznfw-nivel    IS INITIAL
      OR wa_saida_operznfw-aprovador    IS INITIAL
*      or wa_saida_operznfw-dt_val_ate    is initial
*      or wa_saida_operznfw-dt_val_de    is initial
*      or wa_saida_operznfw-hr_val_ate    is initial
*      or wa_saida_operznfw-hr_val_de    is initial
*      or wa_saida_operznfw-motivo    is initial
      .
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_operznfw.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_operznfw-motivo IS INITIAL AND wa_saida_operznfw-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_operznfw.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "/Consistência na Estratégia para data retroativa./
    IF    ( wa_saida_operznfw-ck_ant NE 'X' AND wa_saida_operznfw-dt_val_de < sy-datum )
      OR  ( wa_saida_operznfw-ck_ant NE 'X' AND wa_saida_operznfw-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_operznfw.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.

  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab12.
  ENDIF.


  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_operznfw = it_saida_operznfw_final.

  SORT it_key_operznfw BY dep_resp ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_operznfw COMPARING dep_resp."verificar

  LOOP AT it_key_operznfw INTO wa_key_operznfw.

    "obtendo tabela temporária de acrodo com a chave de estratégia
    it_temp_operznfw = it_saida_operznfw_final.
    DELETE it_temp_operznfw
      WHERE dep_resp       NE wa_key_operznfw-dep_resp.


    "populando a tabela de datas a serem investigadas
    CLEAR: it_dates.

    LOOP AT it_temp_operznfw INTO wa_temp_operznfw.
      wa_dates-date = wa_temp_operznfw-dt_val_de.
      wa_dates-hour = wa_temp_operznfw-hr_val_de.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_operznfw-hr_val_de + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_operznfw-dt_val_de + 1.
        ELSE.
          wa_dates-date = wa_temp_operznfw-dt_val_de.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
      wa_dates-date = wa_temp_operznfw-dt_val_ate.
      wa_dates-hour = wa_temp_operznfw-hr_val_ate.
      APPEND wa_dates TO it_dates.
      IF wa_dates-date NE '99991231' AND wa_dates-hour NE '99991231'.
        wa_dates-hour = wa_temp_operznfw-hr_val_ate + '000001'.
        IF wa_dates-hour EQ '000000'.
          wa_dates-date = wa_temp_operznfw-dt_val_ate + 1.
        ELSE.
          wa_dates-date = wa_temp_operznfw-dt_val_ate.
        ENDIF.
      ENDIF.
      APPEND wa_dates TO it_dates.
      CLEAR wa_dates.
    ENDLOOP.

    SORT it_dates BY date ASCENDING.

    DELETE ADJACENT DUPLICATES FROM it_dates COMPARING date hour.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_dates INTO wa_dates.

      it_loop_operznfw = it_temp_operznfw.

      "IF WA_DATES IS NOT INITIAL.
      DELETE it_loop_operznfw
        WHERE ( dt_val_de < wa_dates-date AND dt_val_ate < wa_dates-date )
          OR  ( dt_val_de > wa_dates-date AND dt_val_ate > wa_dates-date ).
      "ENDIF.

      DELETE it_loop_operznfw
            WHERE dt_val_ate < wa_dates-date "WA_DATES-DATE > DT_VAL_ATE
            OR  dt_val_de > wa_dates-date "WA_DATES-DATE < DT_VAL_DE
            OR ( dt_val_de EQ wa_dates-date AND hr_val_de > wa_dates-hour )"( WA_DATES-DATE EQ DT_VAL_DE AND HR_VAL_DE < WA_DATES-HOUR )
            OR ( dt_val_ate EQ wa_dates-date AND hr_val_ate < wa_dates-hour ). "( WA_DATES-DATE  EQ DT_VAL_ATE AND HR_VAL_ATE > WA_DATES-HOUR ).

      SORT it_loop_operznfw BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_operznfw INTO wa_loop_operznfw.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_operznfw-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_operznfw-nivel IS INITIAL.
          MESSAGE 'Estratégia sem Nível!' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: nivel.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_operznfw, valor_de, valor_ate.
      it_loop_operznfw = it_temp_operznfw.
    ENDLOOP.
    CLEAR: it_dates, it_loop_operznfw.
  ENDLOOP.
  CLEAR: it_temp_operznfw.


  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.


  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_operznfw INTO wa_key_operznfw.

    it_temp_operznfw = it_saida_operznfw_final.

    DELETE it_temp_operznfw
      WHERE dt_val_ate   LT sy-datum
        OR ( dt_val_ate EQ sy-datum AND
             hr_val_ate LT sy-uzeit ).

    DESCRIBE TABLE it_temp_operznfw LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_operznfw.

      it_temp_operznfw = it_saida_operznfw_final.

      DELETE it_temp_operznfw
        WHERE dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      SORT it_temp_operznfw BY nivel dt_val_de dt_val_ate ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_temp_operznfw COMPARING nivel dt_val_de dt_val_ate hr_val_de hr_val_ate.

      DESCRIBE TABLE it_temp_operznfw LINES linha_02.

      IF linha_01 NE linha_02.
        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR: linha_01, linha_02.
        LEAVE TO SCREEN 9000.
      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_var_camb.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_hora_novos_checklist
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_hora_novos_checklist .

  CLEAR: wa_saida_checklist.
  vl_uzeit = sy-uzeit.

  LOOP AT it_saida_checklist INTO wa_saida_checklist WHERE ck_ant NE abap_true.
    wa_saida_checklist-hr_val_de = vl_uzeit.
    wa_saida_checklist-hr_val_ate = '235959'.
    MODIFY it_saida_checklist FROM wa_saida_checklist INDEX sy-tabix.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form consistencia_checklist
*&---------------------------------------------------------------------*
FORM consistencia_checklist .

  LOOP AT it_saida_checklist INTO wa_saida_checklist.
    "/Consistência na Estratégia para falta de dados./
    IF wa_saida_checklist-bukrs        IS INITIAL
      OR wa_saida_checklist-bukrs_ate  IS INITIAL
      OR wa_saida_checklist-vkbur      IS INITIAL
      OR wa_saida_checklist-vkbur_ate  IS INITIAL
      OR wa_saida_checklist-aprovador  IS INITIAL.
      MESSAGE 'Não é possível salvar Estratégias sem dados.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_checklist.
      LEAVE TO SCREEN 9000.
    ENDIF.

    "/Consistência motivo
    IF wa_saida_checklist-motivo IS INITIAL AND wa_saida_checklist-usuario IS INITIAL.
      MESSAGE 'Não é possível salvar, preencha o motivo.' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_checklist.
      LEAVE TO SCREEN 9000.
    ENDIF.

    " validar usuario
    SELECT COUNT(*) FROM usr21
      WHERE bname = wa_saida_checklist-aprovador.

    IF sy-dbcnt = 0.
      MESSAGE 'Aprovador inválido' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_checklist.
      LEAVE TO SCREEN 9000.
    ENDIF.


    "/Consistência na Estratégia para data retroativa./
    IF ( wa_saida_checklist-ck_ant NE 'X' AND wa_saida_checklist-dt_val_de < sy-datum )
      OR  ( wa_saida_checklist-ck_ant NE 'X' AND wa_saida_checklist-dt_val_ate < sy-datum ).
      MESSAGE 'Não é possível salvar Estratégias com data Retroativa!' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_checklist.
      LEAVE TO SCREEN 9000.
    ENDIF.
    "Valida Empresa
    SELECT SINGLE *
      FROM t001
      INTO @DATA(wa_t001_)
      WHERE bukrs =  @wa_saida_checklist-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa DE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_checklist.
      LEAVE TO SCREEN 9000.
    ENDIF.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(wa_t001)
     WHERE bukrs =  @wa_saida_checklist-bukrs_ate.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa ATE, inválida' TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR:  wa_saida_checklist.
      LEAVE TO SCREEN 9000.
    ENDIF.
  ENDLOOP.


  IF v_salvar EQ '8'.
    i_main_tab-pressed_tab = c_main_tab-tab1.
  ENDIF.

  PERFORM agrega_estrategias.

  "Construindo tabela com as chaves de Estratégias
  it_key_checklist = it_saida_checklist_final.

  SORT it_key_checklist BY bukrs     ASCENDING
                           bukrs_ate ASCENDING
                           vkbur     ASCENDING
                           vkbur_ate ASCENDING.

  DELETE ADJACENT DUPLICATES FROM it_key_checklist COMPARING bukrs bukrs_ate vkbur vkbur_ate.

  LOOP AT it_key_checklist INTO wa_key_checklist.

    "obtendo tabela temporária de acordo com a chave de estratégia
    it_temp_checklist = it_saida_checklist_final.
    DELETE it_temp_checklist
      WHERE bukrs     NE wa_key_checklist-bukrs
        OR bukrs_ate  NE wa_key_checklist-bukrs_ate
        OR vkbur      NE wa_key_checklist-vkbur
        OR vkbur_ate  NE wa_key_checklist-vkbur_ate.

    CLEAR: it_bukrs.
    REFRESH: it_bukrs.

    LOOP AT it_temp_checklist INTO wa_temp_checklist.

      wa_bukrs_c-bukrs = wa_temp_checklist-bukrs.
      wa_bukrs_c-bukrs_ate = wa_temp_checklist-bukrs_ate.
      wa_bukrs_c-vkbur = wa_temp_checklist-vkbur.
      wa_bukrs_c-vkbur_ate = wa_temp_checklist-vkbur_ate.
      APPEND wa_bukrs_c TO it_bukrs_c.

    ENDLOOP.

    SORT it_bukrs_c BY bukrs bukrs_ate vkbur vkbur_ate ASCENDING.
    DELETE ADJACENT DUPLICATES FROM it_bukrs_c COMPARING ALL FIELDS.

    "looping na tabela de datas para obter a temp lançamentos dentro da data desejada
    LOOP AT it_bukrs_c INTO wa_bukrs_c.

      it_loop_checklist = it_temp_checklist.

      DELETE it_loop_checklist WHERE bukrs      NE wa_bukrs_c-bukrs
                                 AND bukrs_ate  NE wa_bukrs_c-bukrs_ate
                                 AND vkbur      NE wa_bukrs_c-vkbur
                                 AND vkbur_ate  NE wa_bukrs_c-vkbur_ate.

      wa_dates-hour  = sy-uzeit + 1.

      DELETE it_loop_checklist
        WHERE dt_val_ate LE sy-datum
        AND   hr_val_ate LT wa_dates-hour.

      DELETE it_loop_checklist
       WHERE ( dt_val_ate LT sy-datum OR  dt_val_de GT sy-datum )
       AND   mandt      NE ''.

      DELETE it_loop_checklist
       WHERE dt_val_de  GT sy-datum
       AND   transf_aprov EQ 'P'.

      SORT it_loop_checklist BY nivel ASCENDING.

      "loop na tabela loop para consistência de nível e faixa de valor
      LOOP AT it_loop_checklist INTO wa_loop_checklist.
        "consistência de nível
        CALL FUNCTION 'MOVE_CHAR_TO_NUM'
          EXPORTING
            chr = wa_loop_checklist-nivel
          IMPORTING
            num = nivel.

        IF wa_loop_checklist-nivel IS INITIAL.
          MESSAGE |Estratégia sem Nível Escri { wa_loop_checklist-vkbur } usuário { wa_loop_checklist-aprovador }.| TYPE 'W'.
          LEAVE TO SCREEN 9000.
        ELSE.
          IF sy-tabix EQ 1.
            IF nivel NE 1.
*              MESSAGE 'Estratégia fora de sequência!' TYPE 'S' DISPLAY LIKE 'E'.
              MESSAGE |Estratégia fora de sequência Escri { wa_loop_checklist-vkbur } usuário { wa_loop_checklist-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ELSE.
              nivel_check = nivel.
            ENDIF.
          ELSE.
            IF nivel = nivel_check + 1.
              nivel_check = nivel.
            ELSE.
              MESSAGE |Estratégia fora de sequência Escri { wa_loop_checklist-vkbur } usuário { wa_loop_checklist-aprovador }.| TYPE 'W'.
              CLEAR: nivel, nivel_check.
              LEAVE TO SCREEN 9000.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: nivel_check, wa_loop_checklist, valor_de, valor_ate.
      it_loop_checklist = it_temp_checklist.
    ENDLOOP.
    CLEAR: it_dates, it_loop_checklist.
  ENDLOOP.

  CLEAR: it_temp_checklist.
  "consistência de estratégias duplicadas ou aninhadas.
  CLEAR: c_nivel_min, c_nivel_max, c_range, nivel_max, range_min, range_max.

  "Check de duplicação de estratégia pois temos dois campos para empresa bukrs e bukrs_ate
  LOOP AT it_key_checklist INTO wa_key_checklist.

    it_temp_checklist = it_saida_checklist_final.

    DELETE it_temp_checklist
          WHERE bukrs     NE wa_key_checklist-bukrs
            OR bukrs_ate  NE wa_key_checklist-bukrs_ate
            OR vkbur   NE wa_key_checklist-vkbur
            OR vkbur_ate      NE wa_key_checklist-vkbur_ate
            OR dt_val_ate LT sy-datum
            OR ( dt_val_ate EQ sy-datum AND
                 hr_val_ate LT sy-uzeit ).

    "ALRS
    LOOP AT it_temp_checklist INTO wa_temp_checklist.
      IF wa_temp_checklist-dt_val_de = wa_temp_checklist-dt_val_ate AND
         wa_temp_checklist-hr_val_de = wa_temp_checklist-hr_val_ate.
        wa_temp_checklist-ck_ant = 'D'.
        MODIFY it_temp_checklist FROM wa_temp_checklist INDEX sy-tabix TRANSPORTING ck_ant.
      ENDIF.
    ENDLOOP.
    DELETE it_temp_checklist WHERE ck_ant = 'D'.

    DESCRIBE TABLE it_temp_checklist LINES linha_01.

    IF linha_01 IS NOT INITIAL.

      CLEAR it_temp_checklist.

      it_temp_checklist = it_saida_checklist_final.

      DELETE it_temp_checklist
        WHERE bukrs       < wa_key_checklist-bukrs
          OR bukrs_ate    > wa_key_checklist-bukrs_ate
          OR vkbur     NE wa_key_checklist-vkbur
          OR vkbur_ate        NE wa_key_checklist-vkbur_ate
          OR dt_val_ate   LT sy-datum
          OR ( dt_val_ate EQ sy-datum AND
               hr_val_ate LT sy-uzeit ).

      "ALRS
      LOOP AT it_temp_checklist INTO wa_temp_checklist.
        IF wa_temp_checklist-dt_val_de = wa_temp_checklist-dt_val_ate AND
           wa_temp_checklist-hr_val_de = wa_temp_checklist-hr_val_ate.
          wa_temp_checklist-ck_ant = 'D'.
          MODIFY it_temp_checklist FROM wa_temp_checklist INDEX sy-tabix TRANSPORTING ck_ant.
        ENDIF.
      ENDLOOP.
*      DELETE it_temp_checklist WHERE ck_ant = 'D'.
*
*      SORT it_temp_checklist BY nivel valor_de valor_ate dt_val_de dt_val_ate ASCENDING.
*
*      DELETE ADJACENT DUPLICATES FROM it_temp_checklist COMPARING nivel valor_de valor_ate dt_val_de dt_val_ate hr_val_de hr_val_ate.
*
*      DESCRIBE TABLE it_temp_checklist LINES linha_02.
*
*      IF linha_01 NE linha_02.
*        MESSAGE 'Estratégias Duplicadas e Divergentes!' TYPE 'S' DISPLAY LIKE 'E'.
*        CLEAR: linha_01, linha_02.
*        LEAVE TO SCREEN 9000.
*      ENDIF.

    ENDIF.

    CLEAR: linha_01, linha_02.
  ENDLOOP.

  PERFORM save_data.
  PERFORM edit_alv_checklist.
  PERFORM select_data.
  PERFORM sort_data.
  MESSAGE 'Estratégia Salva com sucesso!' TYPE 'S'.
  "PERFORM SELECT_DATA.

ENDFORM.
