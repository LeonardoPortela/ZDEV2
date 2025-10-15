FUNCTION Z_KSB1_SIS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ANO) TYPE  GJAHR
*"     REFERENCE(I_OFF) TYPE  CHAR1
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_KSB1
*"      T_CCUSTO STRUCTURE  RSPARAMS
*"      T_CLCUSTO STRUCTURE  RSPARAMS
*"----------------------------------------------------------------------
*{   INSERT         DEVK9A1QUY                                        1

  REFRESH resultado.


  TYPES: BEGIN OF ty_objnr,
           objnr_n1 TYPE objnr_n1,
           wogbtr   TYPE wogxxx,
           twaer    TYPE twaer,
         END OF ty_objnr.

  TYPES: BEGIN OF ty_ksb1.
           INCLUDE STRUCTURE  zsys_ksb1.
  TYPES:   wogbtr TYPE zsys_ksb1-wtgbtr,
         END OF ty_ksb1.

  DATA: so_data      TYPE RANGE OF mkpf-budat,
        wa_data      LIKE LINE OF so_data,
        s_ccusto      TYPE RANGE OF kostl,
        s_clcusto      TYPE RANGE OF kstar,
        r_aufnr      TYPE RANGE OF aufnr_neu,
        p_kagru      TYPE kagru,
        wa_ccusto     LIKE LINE OF s_ccusto,
        wa_clcusto     LIKE LINE OF s_ccusto,
        it_selection TYPE TABLE OF rsparams,
        wa_selection LIKE LINE OF it_selection,
        w_resultado  TYPE zsys_ksb1,
        w_resultado2 TYPE ty_ksb1,
        vdatai       TYPE sy-datum,
        vdataf       TYPE sy-datum,
        vccusto_ate   TYPE kostl,
        v_variante   TYPE slis_vari,
        v_objnr      TYPE ty_objnr.

  DATA: lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.

  DATA: it_zim14 TYPE TABLE OF zim14,
        wa_zim14 TYPE zim14,
        vid      TYPE zim14-id.

  cl_salv_bs_runtime_info=>set(
    EXPORTING display  = abap_false
              metadata = abap_false
              data     = abap_true ).


  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.

*  vdatai = |{ i_ano }{ i_mes }01|.
*  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
*    EXPORTING
*      day_in            = vdatai
*    IMPORTING
*      last_day_of_month = vdataf.
*
*  wa_data-sign = 'I'.
*  wa_data-option = 'BT'.
*  wa_data-low = vdatai.
*  wa_data-high = vdataf.
*  APPEND wa_data  TO so_data.

   wa_data-sign = 'I'.
   wa_data-option = 'BT'.
   wa_data-low = |{ i_ano }0101|."vdatai.
   wa_data-high = |{ i_ano }1231|."vdataf.
   APPEND wa_data  TO so_data.

  LOOP AT t_ccusto INTO DATA(w_ccusto) WHERE selname = 'KOSTL'.
    wa_ccusto-sign   = w_ccusto-sign.
    wa_ccusto-option = w_ccusto-option.
    wa_ccusto-low    =  w_ccusto-low.
    wa_ccusto-high   =  w_ccusto-high.
    APPEND wa_ccusto  TO s_ccusto.
  ENDLOOP.

    LOOP AT t_clcusto INTO DATA(w_clcusto) WHERE selname = 'KOSTL'.
    wa_clcusto-sign   = w_clcusto-sign.
    wa_clcusto-option = w_clcusto-option.
    wa_clcusto-low    =  w_clcusto-low.
    wa_clcusto-high   =  w_clcusto-high.
    APPEND wa_clcusto  TO s_clcusto.
  ENDLOOP.

  IF i_off = 'X'.

    p_kagru = 'ORCAMENTO'.

    SUBMIT rkaep000   WITH p_tcode  EQ 'KSB1'
                      "WITH p_kokrs  EQ i_area
                      WITH kostl    IN s_ccusto
                      WITH kstar    IN s_clcusto
                      WITH p_kokrs EQ 'MAGI'
                      WITH r_budat  IN so_data
                      WITH aufnr    IN r_aufnr
                      "WITH KAGRU    EQ P_KAGRU
                      WITH koagr    EQ p_kagru
                      WITH p_maxsel EQ 999999
                      AND RETURN.
    TRY.
        cl_salv_bs_runtime_info=>get_data_ref(
            IMPORTING r_data_descr      = lr_data_descr
                      r_data_line_descr = lr_data_line_descr ).

        IF lr_data_descr IS NOT INITIAL.
          CREATE DATA lr_data TYPE HANDLE lr_data_descr.


          ASSIGN lr_data->* TO <lt_data>.

          cl_salv_bs_runtime_info=>get_data(
            IMPORTING t_data      = <lt_data> ).
        ENDIF.

      CATCH cx_salv_bs_sc_runtime_info.
*      MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
    ENDTRY.

    cl_salv_bs_runtime_info=>clear_all( ).
    IF lr_data_descr IS INITIAL.
      EXIT.
    ENDIF.
    ASSIGN lr_data->* TO <ls_data>.

    LOOP AT <lt_data> ASSIGNING <ls_data>.

      MOVE-CORRESPONDING <ls_data> TO w_resultado.
      MOVE-CORRESPONDING <ls_data> TO w_resultado2.
*      IF i_area = 'MGPY'.
*        MULTIPLY w_resultado2-wogbtr BY 100.
*      ENDIF.
      w_resultado-wtgbtr = w_resultado2-wogbtr.
      MOVE-CORRESPONDING <ls_data> TO v_objnr.

      IF  w_resultado-refbt NE 'R'.
        CONTINUE.
      ENDIF.

      IF w_resultado-wrttp NE '04'.
        CONTINUE.
      ENDIF.

      IF v_objnr-twaer EQ 'USD'.
*        IF i_area = 'MGPY'   .
*          MULTIPLY v_objnr-wogbtr BY 100.
*        ENDIF.
        w_resultado-wtgbtr = v_objnr-wogbtr.
      ENDIF.

      IF v_objnr-objnr_n1+0(3) = 'ORD'.
        w_resultado-aufnr = v_objnr-objnr_n1+4(12).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_resultado-aufnr
          IMPORTING
            output = w_resultado-aufnr.
        SELECT SINGLE auart
          INTO @DATA(v_auart)
          FROM aufk
          WHERE aufnr = @w_resultado-aufnr.
        IF v_auart NE 'ZSTA' AND
           v_auart NE 'ZSIN'.
          CONTINUE.
        ENDIF.
      ELSE.
*      CONTINUE.
      ENDIF.

      SELECT SINGLE ltext FROM cskt INTO w_resultado-nome_centro
           WHERE spras EQ sy-langu AND
                 "kokrs EQ i_area  AND
                 kostl EQ w_resultado-kostl AND
                 datbi GE sy-datum .

      SELECT SINGLE maktx FROM makt INTO w_resultado-maktx
        WHERE spras = sy-langu
        AND   matnr = w_resultado-matnr.

      SELECT SINGLE lifnr INTO w_resultado-lifnr
        FROM coep
        INNER JOIN ekko
        ON ekko~ebeln = coep~ebeln
        WHERE coep~ebeln NE ''
        AND   coep~belnr EQ w_resultado-belnr.
        "AND   coep~kokrs EQ i_area.

      IF sy-subrc = 0.
        SELECT SINGLE name1 INTO w_resultado-name1
          FROM lfa1
          WHERE lifnr = w_resultado-lifnr.
      ENDIF.
      APPEND w_resultado TO resultado.
      CLEAR w_resultado.
    ENDLOOP.

    FREE: <lt_data>.
    FREE: lr_data.
    SELECT MAX( id ) AS id
      INTO vid
      FROM zim14.
    IF vid IS INITIAL.
      vid = 0.
    ELSE.
      ADD 1 TO vid.
    ENDIF.

    LOOP AT resultado INTO w_resultado.
      add 1 to vid.
      MOVE-CORRESPONDING w_resultado TO wa_zim14.
      wa_zim14-id = vid.
      wa_zim14-ano = i_ano.
      "wa_zim14-mes = i_mes.
      "wa_zim14-area = i_area.
      APPEND wa_zim14 TO it_zim14.
    ENDLOOP.
    "DELETE FROM zim14 WHERE mes   = i_mes AND   ano   = i_ano.
                      "AND   area  = i_area.
    "
    "MODIFY  zim14 FROM TABLE it_zim14.
    COMMIT WORK.
  ELSE.
    SELECT *
      FROM zim14
      INTO TABLE it_zim14
      WHERE ano   = i_ano.
      "AND   mes   = i_mes
      "AND   area  = i_area.
    LOOP AT it_zim14 INTO wa_zim14.
      MOVE-CORRESPONDING wa_zim14 TO w_resultado.
      APPEND w_resultado TO resultado.

    ENDLOOP.
  ENDIF.


*}   INSERT
ENDFUNCTION.
