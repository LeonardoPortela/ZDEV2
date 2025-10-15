*&---------------------------------------------------------------------*
*&  Include  ZSDR0060_5822
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& variaveis locais
*&---------------------------------------------------------------------*
DATA: l_leave_5822   TYPE c.

*&---------------------------------------------------------------------*
*&  Include  ZSDR0060_5822
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Editar RTC
*&---------------------------------------------------------------------*
FORM f_edita_rtc CHANGING p_carga_5820 TYPE ty_carga_5820.

  TYPES: BEGIN OF ty_0298,
           nro_cgd       TYPE znro_cg,
           ch_referencia TYPE zch_ref,
           id            TYPE string.
  TYPES: END OF ty_0298.

  DATA: t_0298    TYPE TABLE OF ty_0298,
        w_0298    TYPE ty_0298,
        w_0139    TYPE zsdt0139,
        l_receita TYPE string.

  FREE: l_receita, l_leave_5822.

  l_carga_5820_nro_cgd = p_carga_5820-nro_cgd.

*----------------------
* Verifica se pode editar RTC
*----------------------
  SELECT nro_cgd, nro_sol, seq, vbeln
    FROM zsdt0140
    INTO TABLE @DATA(t_0140)
   WHERE nro_cgd = @p_carga_5820-nro_cgd.

  IF t_0140[] IS NOT INITIAL.
*-CS2021000218-05.10.2022-#91290-JT-inicio
    SELECT *
      FROM zsdt0082
      INTO CORRESPONDING FIELDS OF TABLE it_sol_5740
       FOR ALL ENTRIES IN t_0140
     WHERE   nro_sol          = t_0140-nro_sol
       AND   zsdt0082~seq    NE 1
       AND ( zsdt0082~status EQ 2
        OR   zsdt0082~status EQ 5 ).
*-CS2021000218-05.10.2022-#91290-JT-fim

    SELECT vbeln, posnr, charg, nro_cg, nr_fase, nr_rot, ch_referencia
      FROM zsdt0134
      INTO TABLE @DATA(t_0134)
       FOR ALL ENTRIES IN @t_0140
     WHERE nro_cg  = @t_0140-nro_cgd
       AND vbeln   = @t_0140-vbeln.

    IF t_0134[] IS NOT INITIAL.
      SELECT nro_cgd ch_referencia id
        FROM zsdt0298
        INTO TABLE t_0298
         FOR ALL ENTRIES IN t_0134
       WHERE ch_referencia = t_0134-ch_referencia
         AND cancelado     = abap_false.
    ENDIF.

    IF t_0298[] IS NOT INITIAL.
      LOOP AT t_0298 INTO w_0298.
        l_receita = l_receita && w_0298-id && ','.
      ENDLOOP.
      MESSAGE s024(sd) WITH text-190 l_receita text-191 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

*----------------------
* recupera RTC
*----------------------
  SELECT SINGLE *
    FROM zsdt0139
    INTO w_0139
   WHERE nro_cgd = p_carga_5820-nro_cgd.

  wa_header_cargad-cod_ar     = w_0139-cod_ar.
  wa_header_cargad-cpf_rtc    = w_0139-cpf_rtc.
  l_cpf_rtc                   = w_0139-cpf_rtc.

  IF w_0139-tipo_rtc = 'T'.
    w_rtc_proprio             = abap_false.
    w_rtc_terceiro            = abap_true.
    wa_header_cargad-tipo_rtc = 'T'.
  ELSE.
    w_rtc_proprio             = abap_true.
    w_rtc_terceiro            = abap_false.
    wa_header_cargad-tipo_rtc = 'P'.
  ENDIF.

*----------------------
* editar RTC
*----------------------
  CALL SCREEN 5822 STARTING AT 38  4
                     ENDING AT 144 5.

  CHECK l_leave_5822 = abap_false.

*----------------------
* Atualiza GRID com nome RTC
*----------------------
  CLEAR wa_header_cargad-nome_rtc.
  SELECT SINGLE nome
    FROM zsdt0259
    INTO wa_header_cargad-nome_rtc
   WHERE cpf EQ wa_header_cargad-cpf_rtc.

  p_carga_5820-nome_rtc = wa_header_cargad-nome_rtc.
  p_carga_5820-tipo_rtc = wa_header_cargad-tipo_rtc.

  IF p_carga_5820-tipo_rtc = 'T'.
    p_carga_5820-tipo_rtc2 = 'Terceiro'.
  ELSE.
    p_carga_5820-tipo_rtc2 = 'Proprio'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5822  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5822 OUTPUT.

  CLEAR ok_code2.
  SET PF-STATUS 'STATUS_5822'.
  SET TITLEBAR 'STATUS_5822'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5822  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_5822 INPUT.

  CASE ok_code2.
    WHEN 'SALVAR'.
      PERFORM f_salva_rtc USING l_carga_5820_nro_cgd.

      IF vl_check = abap_false.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      l_leave_5822 = abap_true.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_5822  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_5822_trata_cpf OUTPUT.

  IF wa_header_cargad-cpf_rtc IS NOT INITIAL AND
     wa_header_cargad-cpf_rtc <> '00000000000'.
    l_cpf_rtc = wa_header_cargad-cpf_rtc.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'WA_HEADER_CARGAD-CPF_RTC'.
      IF w_rtc_terceiro = abap_true.
        screen-input             = '0'.
        wa_header_cargad-cpf_rtc = abap_off.
      ELSE.
        screen-input             = '1'.
        wa_header_cargad-cpf_rtc = l_cpf_rtc.
      ENDIF.
      MODIFY SCREEN.

      CLEAR wa_header_cargad-nome_rtc.
      SELECT SINGLE nome
        FROM zsdt0259
        INTO wa_header_cargad-nome_rtc
       WHERE cpf EQ wa_header_cargad-cpf_rtc.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*& SALVAR TRC
*&---------------------------------------------------------------------*
FORM f_salva_rtc USING p_nro_cgd.

  DATA: wa_zsdt0259 TYPE zsdt0259,
        wa_zsdt0266 TYPE zsdt0266.

  CLEAR vl_check.

  IF w_rtc_proprio = abap_true.
    wa_header_cargad-tipo_rtc = 'P'.
  ELSE.
    wa_header_cargad-tipo_rtc = 'T'.
    wa_header_cargad-cpf_rtc  = abap_off.
  ENDIF.

*-CS2021000218-05.10.2022-#91290-JT-inicio
*-monta range centro
  FREE: r_werks.
  LOOP AT it_sol_5740 INTO wa_sol_5740.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = wa_sol_5740-werks.
    APPEND r_werks.
  ENDLOOP.
  IF r_werks[] IS INITIAL.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = '9999'.
    APPEND r_werks.
  ENDIF.
  SORT r_werks BY low.
  DELETE ADJACENT DUPLICATES FROM r_werks
                        COMPARING low.
*-CS2021000218-05.10.2022-#91290-JT-fim

  IF wa_header_cargad-cpf_rtc  IS NOT INITIAL   AND
     wa_header_cargad-cpf_rtc  <> '00000000000' AND
     wa_header_cargad-tipo_rtc  = 'P'.
    SELECT SINGLE *
             INTO wa_zsdt0259
             FROM zsdt0259
            WHERE cpf = wa_header_cargad-cpf_rtc.

    IF sy-subrc <> 0.
      MESSAGE text-160 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ELSEIF wa_zsdt0259-status = 'N'.
      MESSAGE text-161 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ELSE.
      SELECT SINGLE *
               INTO wa_zsdt0266
               FROM zsdt0266
              WHERE cpf     = wa_header_cargad-cpf_rtc
                AND werks  IN r_werks.  "*-CS2021000218-05.10.2022-#91290-JT-fim
      IF sy-subrc <> 0.
        MESSAGE text-162 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDIF.
  ELSE.
    IF wa_header_cargad-tipo_rtc = 'P'.
      MESSAGE text-180 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ENDIF.
  ENDIF.

  CHECK vl_check = abap_false.

*----------------------
* Atualiza RTC
*----------------------
  UPDATE zsdt0139 SET cpf_rtc  = wa_header_cargad-cpf_rtc
                      tipo_rtc = wa_header_cargad-tipo_rtc "*-CS2021000218-30.08.2022-#893743-JT
                WHERE nro_cgd  = l_carga_5820_nro_cgd.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*&   search help cpf
*&---------------------------------------------------------------------*
FORM f_search_cpf_rtc USING p_modo.

  TYPES: BEGIN OF ty_data,
           cpf  TYPE zsdt0259-cpf,
           nome TYPE zsdt0259-nome,
         END   OF ty_data.

  DATA: t_zsdt0259   TYPE TABLE OF zsdt0259,
        w_zsdt0259   TYPE zsdt0259,
        t_zsdt0266   TYPE TABLE OF zsdt0266,
        t_dynpfields TYPE TABLE OF dynpread,
        w_dynpfields TYPE dynpread,
        t_data       TYPE TABLE OF ty_data,
        w_data       TYPE ty_data,
        t_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        t_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  FREE: t_data, t_dynpfields.

*-CS2021000218-05.10.2022-#91290-JT-inicio
*-monta range centro
  FREE: r_werks.

  CASE p_modo.
    WHEN '5730'.
      LOOP AT it_sol_5730 INTO wa_sol_5730.
        r_werks-sign   = 'I'.
        r_werks-option = 'EQ'.
        r_werks-low    = wa_sol_5730-werks.
        APPEND r_werks.
      ENDLOOP.

    WHEN '5740'.
      LOOP AT it_sol_5740 INTO wa_sol_5740.
        r_werks-sign   = 'I'.
        r_werks-option = 'EQ'.
        r_werks-low    = wa_sol_5740-werks.
        APPEND r_werks.
      ENDLOOP.
  ENDCASE.

  IF r_werks[] IS INITIAL.
    r_werks-sign   = 'I'.
    r_werks-option = 'EQ'.
    r_werks-low    = '9999'.
    APPEND r_werks.
  ENDIF.
  SORT r_werks BY low.
  DELETE ADJACENT DUPLICATES FROM r_werks
                        COMPARING low.
*-CS2021000218-05.10.2022-#91290-JT-fim

  SELECT * FROM zsdt0266
           INTO TABLE t_zsdt0266
          WHERE werks IN r_werks.  "*-CS2021000218-05.10.2022-#91290-JT-fim

  IF t_zsdt0266[] IS INITIAL.
    SELECT * FROM zsdt0259
             INTO TABLE t_zsdt0259
            WHERE status = 'S'.
  ELSE.
    SELECT * FROM zsdt0259
             INTO TABLE t_zsdt0259
              FOR ALL ENTRIES IN t_zsdt0266
            WHERE cpf    = t_zsdt0266-cpf
              AND status = 'S'.
  ENDIF.

  LOOP AT t_zsdt0259            INTO w_zsdt0259.
    MOVE-CORRESPONDING w_zsdt0259 TO w_data.
    APPEND w_data                 TO t_data.
  ENDLOOP.

  CHECK t_data[] IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CPF'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WA_HEADER_CARGAD-CPF_RTC'
      value_org       = 'S'
    TABLES
      value_tab       = t_data
      return_tab      = t_return_tab
      dynpfld_mapping = t_dselc.

  READ TABLE t_return_tab INDEX 1.

  IF sy-subrc = 0.
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
      EXPORTING
        input  = t_return_tab-fieldval
      IMPORTING
        output = wa_header_cargad-cpf_rtc.

    MOVE: 'WA_HEADER_CARGAD-CPF_RTC'  TO w_dynpfields-fieldname,
           wa_header_cargad-cpf_rtc   TO w_dynpfields-fieldvalue.
    APPEND w_dynpfields               TO t_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = t_dynpfields.
  ENDIF.

ENDFORM.
