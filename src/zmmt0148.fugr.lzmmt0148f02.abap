*----------------------------------------------------------------------*
***INCLUDE LZMMT0148F02.
*----------------------------------------------------------------------*
FORM z_validar_centro.
  DATA lwa_row TYPE zmmt0148.

  LOOP AT total.

    IF <vim_total_struc> IS ASSIGNED.
      MOVE-CORRESPONDING <vim_total_struc> TO lwa_row.
    ENDIF.

    IF <action> NE 'D' AND <action> IS NOT INITIAL AND <action> NE 'X'.
      IF lwa_row-bukrs IS NOT INITIAL.

        IF lwa_row-centro_virt IS  INITIAL.
          MESSAGE 'Informar Centro Virtual' TYPE 'S' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          vim_abort_saving = 'X'.
          EXIT.
        ENDIF.


        SELECT SINGLE *
          FROM t001w INTO @DATA(_wl_t001w_v)
         WHERE werks = @lwa_row-centro_virt.

        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(_wl_j_1bbranch_v)
         WHERE bukrs  = @lwa_row-bukrs
           AND branch = @_wl_t001w_v-j_1bbranch.

        IF _wl_j_1bbranch_v IS INITIAL.
          MESSAGE 'Centro informado não peretence a essa empresa' TYPE 'S' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          vim_abort_saving = 'X'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM t001w INTO @DATA(_wl_t001w_r)
         WHERE werks = @lwa_row-centro_ref.

        IF lwa_row-centro_ref IS  INITIAL.
          MESSAGE 'Informar Centro de Referencia' TYPE 'S' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          vim_abort_saving = 'X'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(_wl_j_1bbranch_ref)
         WHERE bukrs  = @lwa_row-bukrs
           AND branch = @_wl_t001w_r-j_1bbranch.

        IF _wl_j_1bbranch_ref IS INITIAL.
          MESSAGE 'Centro informado não peretence a essa empresa'  TYPE 'S' DISPLAY LIKE 'E'.
          sy-subrc = 4.
          vim_abort_saving = 'X'.
          EXIT.
        ENDIF.

        CLEAR: lwa_row,
       _wl_t001w_v,
       _wl_j_1bbranch_v,
       _wl_t001w_r,
       _wl_j_1bbranch_ref.


      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
