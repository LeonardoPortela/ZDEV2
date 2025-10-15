*&---------------------------------------------------------------------*
*&  Include           ZHCMR_PA0105_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  TYPES: BEGIN OF ty_pernr,
           pernr TYPE pernr,
         END OF ty_pernr.

  DATA: lt_pernr TYPE TABLE OF ty_pernr.
  DATA: wa_pernr TYPE ty_pernr.

  DATA: r_pernr TYPE RANGE OF pernr.
  DATA: wa_pernr_aux LIKE LINE OF r_pernr.

  DATA: r_empresas_ignoradas TYPE RANGE OF pa0001-bukrs.

  SELECT valsign AS sign
         valoption AS option
         valfrom AS low
    FROM setleaf
    INTO TABLE r_empresas_ignoradas
    WHERE setname = 'ZHCM_EMPRESA_DIALOG'.

  IF p_data IS INITIAL.
    p_data = sy-datum.
  ENDIF.


  IF p_ativo IS NOT INITIAL.

    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
      INTO TABLE t_pa001
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
      ON b~pernr = a~pernr  AND
         b~endda >= p_data AND
         b~stat2 <> '0'
    WHERE a~pernr IN so_matri  AND
          a~bukrs IN so_empre AND
          a~endda >= p_data   AND
          a~abkrs <> 'BA'.
    IF sy-subrc IS INITIAL.
      SORT t_pa001 BY pernr
                       stell
                       bukrs
                       werks
                       orgeh
                       stat2.
      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING ALL FIELDS.
      DELETE t_pa001 WHERE bukrs IN r_empresas_ignoradas.
    ENDIF.


  ELSEIF p_attem IS NOT INITIAL.

    p_data = sy-datum - 2.

    SELECT pernr
      FROM pa0465
      INTO TABLE @DATA(lt_pa0465)
      WHERE begda >= @p_data OR endda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0465 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0465 COMPARING ALL FIELDS.

      LOOP AT lt_pa0465 INTO DATA(wa_pa0465).
        wa_pernr-pernr = wa_pa0465-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.


    SELECT pernr
       FROM pa0001
       INTO TABLE @DATA(lt_pa0001)
       WHERE begda >= @p_data OR endda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0001 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0001 COMPARING ALL FIELDS.

      LOOP AT lt_pa0001 INTO DATA(wa_pa0001).
        wa_pernr-pernr = wa_pa0001-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

    SELECT pernr
       FROM pa0002
       INTO TABLE @DATA(lt_pa0002)
       WHERE begda >= @p_data OR endda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0002 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0002 COMPARING ALL FIELDS.

      LOOP AT lt_pa0002 INTO DATA(wa_pa0002).
        wa_pernr-pernr = wa_pa0002-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

    SELECT pernr
       FROM pa0000
       INTO TABLE @DATA(lt_pa0000)
       WHERE begda >= @p_data OR endda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0000 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0000 COMPARING ALL FIELDS.

      LOOP AT lt_pa0000 INTO DATA(wa_pa0000).
        wa_pernr-pernr = wa_pa0000-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

    SELECT pernr
       FROM pa0105
       INTO TABLE @DATA(lt_pa0105)
       WHERE begda >= @p_data OR endda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa0105 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa0105 COMPARING ALL FIELDS.

      LOOP AT lt_pa0105 INTO DATA(wa_pa0105).
        wa_pernr-pernr = wa_pa0105-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

    SELECT pernr
       FROM pa9002
       INTO TABLE @DATA(lt_pa9002)
       WHERE begda >= @p_data OR endda >= @p_data OR aedtm >= @p_data.

    IF sy-subrc IS INITIAL.
      SORT lt_pa9002 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pa9002 COMPARING ALL FIELDS.

      LOOP AT lt_pa9002 INTO DATA(wa_pa9002).
        wa_pernr-pernr = wa_pa9002-pernr.

        APPEND wa_pernr TO lt_pernr.
        CLEAR: wa_pernr.
      ENDLOOP.
    ENDIF.

    SORT lt_pernr BY pernr.
    DELETE ADJACENT DUPLICATES FROM lt_pernr COMPARING pernr.

    LOOP AT lt_pernr INTO wa_pernr.

      wa_pernr_aux-sign   = 'I'.
      wa_pernr_aux-option = 'EQ'.
      wa_pernr_aux-low    = wa_pernr-pernr.

      APPEND wa_pernr_aux TO r_pernr.
      CLEAR: wa_pernr_aux.

    ENDLOOP.

    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
      INTO TABLE t_pa001
      FROM pa0001 AS a
      INNER JOIN pa0000 AS b
      ON b~pernr = a~pernr  AND
         b~endda >= p_data
     WHERE a~pernr IN so_matri  AND
          a~bukrs  IN so_empre AND
          a~endda >= p_data   AND
          a~abkrs <> 'BA'.

    IF sy-subrc IS INITIAL.

      SORT t_pa001 BY pernr
                       stell
                       bukrs
                       werks
                       orgeh
                       stat2.

      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING ALL FIELDS.
      DELETE t_pa001 WHERE pernr NOT IN r_pernr.
      DELETE t_pa001 WHERE bukrs IN r_empresas_ignoradas.

    ENDIF.

  ELSEIF p_inat IS NOT INITIAL.

    SELECT a~pernr
           a~stell
           a~bukrs
           a~werks
           a~orgeh
           b~stat2
    INTO TABLE t_pa001
    FROM pa0001 AS a
    INNER JOIN pa0000 AS b
    ON b~pernr = a~pernr  AND
       b~endda >= p_data AND
       b~stat2 <> '0'
    WHERE a~pernr IN so_matri  AND
       a~bukrs IN so_empre AND
       a~endda >= p_data   AND
       a~abkrs <> 'BA'.

    IF sy-subrc IS INITIAL.

      SORT t_pa001 BY pernr
                       stell
                       bukrs
                       werks
                       orgeh
                       stat2.

      DELETE ADJACENT DUPLICATES FROM t_pa001 COMPARING ALL FIELDS.
      DELETE t_pa001 WHERE bukrs IN r_empresas_ignoradas.
    ENDIF.

  ENDIF.

  IF t_pa001 IS NOT INITIAL.

    DATA(obj_carga) = NEW carga_sf( ).

    v_check_job = obj_carga->get_background_job( ).

    IF v_check_job IS INITIAL.

      CALL SCREEN 0100.

    ENDIF.

  ELSE.
    MESSAGE 'Sem resultado para os filtros informados!' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
