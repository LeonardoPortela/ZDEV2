*----------------------------------------------------------------------*
***INCLUDE LZMMT0148F01.
*----------------------------------------------------------------------*
FORM F_VALIDATE_ENTRY.

  DATA lwa_row TYPE zmmt0148.
 BREAK-POINT.
  LOOP AT TOTAL.
    CLEAR lwa_row.
    BREAK-POINT.
    IF <vim_total_struc> IS ASSIGNED.
      MOVE-CORRESPONDING <vim_total_struc> TO lwa_row.
    ENDIF.

    IF <action> NE 'D' AND <action> IS NOT INITIAL AND <action> NE 'X'.
      IF lwa_row-bukrs IS NOT INITIAL.


        SELECT SINGLE *
          FROM t001w INTO @DATA(_wl_t001w_v)
         WHERE werks = @lwa_row-centro_virt.

        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(_wl_j_1bbranch_v)
         WHERE bukrs  = @lwa_row-bukrs
           AND branch = @_wl_t001w_v-j_1bbranch.

        IF _wl_j_1bbranch_v IS INITIAL.
          MESSAGE 'Currency must be USD or IDR' TYPE 'S' DISPLAY LIKE 'E'.
          vim_abort_saving = 'X'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM t001w INTO @DATA(_wl_t001w_r)
         WHERE werks = @lwa_row-centro_ref.

        SELECT SINGLE *
          FROM j_1bbranch INTO @DATA(_wl_j_1bbranch_ref)
         WHERE bukrs  = @lwa_row-bukrs
           AND branch = @_wl_t001w_v-j_1bbranch.

        IF _wl_j_1bbranch_ref IS INITIAL.
          MESSAGE 'Currency must be USD or IDR' TYPE 'S' DISPLAY LIKE 'E'.
          vim_abort_saving = 'X'.
          EXIT.
        ENDIF.


      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
