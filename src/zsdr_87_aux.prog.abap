*&---------------------------------------------------------------------*
*& Report ZSDR_87_AUX
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr_87_aux.

TABLES zsdt0059.

PARAMETERS p_nro TYPE zsdt0059-nro_sol_ov OBLIGATORY.
PARAMETERS p_posnr TYPE zsdt0059-posnr.
PARAMETERS p_tpcalc TYPE zsdt0059-tipo_calc.

START-OF-SELECTION.

  DATA lv_bez_c TYPE c LENGTH 2.
  DATA lr_bezei TYPE RANGE OF bezei.

  DO 15 TIMES.

    CHECK sy-index > 1.

    MOVE sy-index TO lv_bez_c.

    APPEND 'IEQC' && lv_bez_c TO lr_bezei.
    APPEND 'IEQT' && lv_bez_c TO lr_bezei.
    APPEND 'IEQP' && lv_bez_c TO lr_bezei.

  ENDDO.

  BREAK-POINT.

  SELECT * FROM zsdt0059
    INTO TABLE @DATA(lt_0059)
    WHERE nro_sol_ov = @p_nro
      AND posnr = @p_posnr
      AND tipo_calc = 'V'
      AND bezei IN @lr_bezei.

  CHECK lt_0059 IS NOT INITIAL.

  LOOP AT lt_0059 ASSIGNING FIELD-SYMBOL(<fs_0059>).

    CLEAR: <fs_0059>-formula, <fs_0059>-formula2,
           <fs_0059>-cbot, <fs_0059>-valdt,
           <fs_0059>-monat, <fs_0059>-posnr1,
           <fs_0059>-valdt, <fs_0059>-preco,
           <fs_0059>-safra.

  ENDLOOP.

  MODIFY zsdt0059 FROM TABLE lt_0059.

  COMMIT WORK AND WAIT.
